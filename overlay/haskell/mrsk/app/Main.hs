{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Cli (
  CliEffect,
  askConfirmExit,
  runCliEffect,
  tellBuildFinished,
  tellBuildStarted,
  tellFoundConfigurations,
 )
import Cli qualified
import Control.Exception (Exception, SomeException, throw)
import Control.Monad (foldM, forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Key qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text qualified as A
import Data.Aeson.Types qualified as A
import Data.Either (fromLeft, fromRight, rights)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.Vector qualified as V
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (Concurrently (..))
import Effectful.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Exception (ExceptionWithContext, bracket_, catch, finally, throwIO)
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO (stdout)
import Effectful.FileSystem.IO.ByteString (hGetLine, hPutStr)
import Effectful.Monad.Logger (
  Logger,
  logDebugN,
  logErrorN,
  logInfoN,
  runConfigurationLogging,
  runFileLogging,
  runOtherTerminalLogging,
  runStderrLogging,
  runVoidLogging,
 )
import Effectful.Prim (Prim, runPrim)
import Effectful.Process.Typed (ExitCode, TypedProcess, runTypedProcess)
import GHC.Generics (Generic)
import GHC.Stack.Types (HasCallStack)
import Nix (Nix, nixBuild, nixEval, nixSelect, runNixEffect)
import Nix.Select (NixTarget (NixDerivation, NixFlake), Selector (..), nixFlakeInfo, select)
import Options (Command (..), Logging (..), Options (Options, command, logging, nom), options)
import Options.Applicative (execParser)
import Path.Posix (Abs, Dir, File, Path, absdir, parseAbsDir, parseAbsFile, toFilePath)
import Remote (
  Near (..),
  RemoteEffect,
  SwitchAction (SwitchAction'DryActivate, SwitchAction'Switch),
  near,
  openConnection,
  runRemote,
 )
import Remote.Far (serveFar)
import RequireCallStack (RequireCallStack, provideCallStack)

data NixOSConfiguration
  = NixOSConfiguration
  { targetHost :: Text
  , remoteUser :: Text
  }
  deriving stock (Generic)
  deriving anyclass (A.FromJSON)

getNixOSConfigurationMetadata :: (Nix :> es) => [Text] -> Text -> Eff es (HashMap Text (Maybe NixOSConfiguration))
getNixOSConfigurationMetadata hosts flake =
  nixSelect
    [
      (
        [ SelectorStr "nixosConfigurations"
        , SelectorSet (map SelectorStr hosts)
        , SelectorStr "config"
        , SelectorMaybe "mrsk"
        , SelectorAll
        ]
      , \case
          A.Object km -> A.Object $ (`KM.map` km) \case
            A.Object (KM.toList -> []) -> A.Null
            A.Object (KM.toList -> [("mrsk", inner)]) -> inner
            other -> other
          other -> other
      )
    ]
    flake
    <&> head

getNixOSConfigurations :: (Nix :> es) => Maybe [Text] -> Text -> Eff es [Text]
getNixOSConfigurations hosts flake =
  nixSelect
    [
      (
        [ SelectorStr "nixosConfigurations"
        , maybe SelectorAll (SelectorSet . map SelectorStr) hosts
        , SelectorMaybe ""
        ]
      , \case
          A.Object km -> A.Array . V.fromList . map (A.String . AK.toText) $ KM.keys km
          other -> other
      )
    ]
    flake
    <&> head

traverseThrottled
  :: (Concurrent :> es, Traversable t)
  => Int -> (a -> Eff es b) -> t a -> Eff es (t (Either (ExceptionWithContext SomeException) b))
traverseThrottled concLevel action taskContainer = do
  sem <- newQSem concLevel
  let
    throttledAction = bracket_ (waitQSem sem) (signalQSem sem) . action
    caughtAction task = catch (throttledAction task <&> Right) (pure . Left)
  runConcurrently (traverse (Concurrently . caughtAction) taskContainer)

type TopLevelEffects =
  [ RemoteEffect
  , Environment
  , Nix
  , CliEffect
  , Logger
  , TypedProcess
  , FileSystem
  , Concurrent
  , Fail
  , Prim
  , IOE
  ]

logExceptions :: (Logger :> es, RequireCallStack) => [Either (ExceptionWithContext SomeException) a] -> Eff es [a]
logExceptions results = foldM logException [] results
 where
  logException :: (Logger :> es, RequireCallStack) => [a] -> Either (ExceptionWithContext SomeException) a -> Eff es [a]
  logException accumulator (Left exception) = do
    logErrorN ("operation failed with exception " <> T.show exception)
    pure accumulator
  logException accumulator (Right value) = pure (value : accumulator)

mrsk :: (RequireCallStack) => Options -> Eff TopLevelEffects ()
mrsk Options{command = Far} =
  serveFar `catch` \(exception :: SomeException) ->
    logErrorN ("Far side caught exception: " <> T.show exception)
mrsk Options{command = Deploy{hosts, flake, sudo, action}} = do
  configurations :: [Text] <- case hosts of
    Just hosts' -> getNixOSConfigurations (Just (NE.toList hosts')) flake
    Nothing -> getNixOSConfigurations Nothing flake

  logDebugN ("found these configurations: [ " <> T.intercalate ", " configurations <> " ]")

  configurationMetadatas :: HashMap Text NixOSConfiguration <-
    getNixOSConfigurationMetadata configurations flake <&> HM.mapMaybe id

  logDebugN
    ("found these configurations with mrsk metadata: [ " <> T.intercalate ", " (HM.keys configurationMetadatas) <> " ]")

  tellFoundConfigurations (HM.keys configurationMetadatas)

  derivations :: HashMap Text (Path Abs File) <-
    HM.fromList
      <$> ( logExceptions
              =<< traverseThrottled
                4
                ( \name -> runConfigurationLogging name do
                    logDebugN "starting evaluation"

                    let attr = "nixosConfigurations.\"" <> name <> "\".config.system.build.toplevel.drvPath"
                    result <-
                      (nixEval @Text . NixFlake $ flake <> "#" <> attr)
                        >>= parseAbsFile . T.unpack
                        <&> (name,)

                    logDebugN "finished evaluation" *> pure result
                )
                (HM.keys configurationMetadatas)
          )

  builtPaths :: HashMap Text (Path Abs Dir) <-
    HM.fromList
      <$> ( logExceptions
              =<< traverseThrottled
                4
                ( \(name, derivation) ->
                    runConfigurationLogging name do
                      logDebugN "starting build"
                      tellBuildStarted name

                      result <-
                        ( nixBuild (NixDerivation derivation)
                            <&> (name,) . fromLeft undefined . head
                        )

                      tellBuildFinished name
                      logDebugN "finished build" *> pure result
                )
                (HM.toList derivations)
          )

  deployedConfigurations :: HashMap Text (ExitCode, Text) <-
    HM.fromList
      <$> ( logExceptions
              =<< traverseThrottled
                4
                ( \(name, NixOSConfiguration{remoteUser, targetHost}) -> runConfigurationLogging name do
                    logDebugN "starting deploy"

                    handle <- openConnection name remoteUser targetHost

                    logDebugN "Remote connection opened"

                    output <-
                      near handle $
                        Near'SwitchNixos
                          { configuration = (builtPaths HM.! name)
                          , action
                          , sudo
                          }

                    logDebugN "finished deploy" *> pure (name, output)
                )
                (HM.toList configurationMetadatas)
          )

  forM_ (HM.toList deployedConfigurations) \(name, (exitCode, output)) ->
    logInfoN (name <> " deployed with " <> T.show exitCode <> " and " <> output)

  pure ()

main :: IO ()
main =
  provideCallStack $
    liftIO (execParser options) >>= \opts@Options{logging, nom} ->
      effectStack logging nom $ (mrsk opts >> askConfirmExit Nothing) `catch` (askConfirmExit . Just)
 where
  effectStack
    :: (RequireCallStack)
    => Logging
    -> Bool
    -> Eff TopLevelEffects a
    -> IO a
  effectStack logging nom =
    runEff
      . runPrim
      . runFailIO
      . runConcurrent
      . runFileSystem
      . runTypedProcess
      . ( case logging of
            Logging'OtherTerminal -> runOtherTerminalLogging
            Logging'File path -> runFileLogging path
            Logging'Blackhole -> runVoidLogging
            Logging'Stderr -> runStderrLogging
        )
      . runCliEffect Cli.Config{nom}
      . runNixEffect
      . runEnvironment
      . runRemote
