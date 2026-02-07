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
  tellBuildStarted,
  tellDeploymentFinished,
  tellDeploymentStarted,
  tellEvaluationStarted,
  tellException,
 )
import Cli qualified
import Control.Exception (SomeException)
import Control.Monad (foldM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as A
import Data.Either (fromLeft)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (Concurrently (..))
import Effectful.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Exception (ExceptionWithContext, bracket_, catch, throwIO)
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem (FileSystem, runFileSystem)
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
import Nix (Nix, nixBuild, nixEval, nixSelect, runNixEffect)
import Nix.Select (NixTarget (NixDerivation, NixFlake), Selector (..))
import Options (Command (..), Logging (..), Options (Options, command, logging, nom), options)
import Options.Applicative (execParser)
import Path.Posix (Abs, Dir, File, Path, parseAbsFile)
import Remote (
  Near (..),
  RemoteEffect,
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
  :: (Concurrent :> es)
  => Int
  -> (Text -> a -> Eff es b)
  -> HashMap Text a
  -> Eff es (HashMap Text (Either (ExceptionWithContext SomeException) b))
traverseThrottled concLevel action taskContainer = do
  sem <- newQSem concLevel
  let
    throttledAction = bracket_ (waitQSem sem) (signalQSem sem) . uncurry action
    caughtAction task = catch (throttledAction task <&> Right) (pure . Left)
  runConcurrently (HM.traverseWithKey (\key value -> Concurrently $ caughtAction (key, value)) taskContainer)

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

logExceptions
  :: (CliEffect :> es, Logger :> es, RequireCallStack)
  => HashMap Text (Either (ExceptionWithContext SomeException) a) -> Eff es (HashMap Text a)
logExceptions results = HM.foldlWithKey' logException (pure HM.empty) results
 where
  logException
    :: (CliEffect :> es, Logger :> es, RequireCallStack)
    => Eff es (HashMap Text a) -> Text -> Either (ExceptionWithContext SomeException) a -> Eff es (HashMap Text a)
  logException action machine (Left exception) =
    action >>= \accumulator -> do
      logErrorN ("operation failed with exception " <> T.show exception)
      tellException machine exception
      pure accumulator
  logException action machine (Right value) = action >>= \accumulator -> pure $ HM.insert machine value accumulator

mrsk :: (RequireCallStack) => Options -> Eff TopLevelEffects ()
mrsk Options{command = Far} = undefined
mrsk Options{command = Deploy{hosts, flake, sudo, action}} = do
  configurations :: [Text] <- case hosts of
    Just hosts' -> getNixOSConfigurations (Just (NE.toList hosts')) flake
    Nothing -> getNixOSConfigurations Nothing flake

  logDebugN ("found these configurations: [ " <> T.intercalate ", " configurations <> " ]")

  configurationMetadatas :: HashMap Text NixOSConfiguration <-
    getNixOSConfigurationMetadata configurations flake <&> HM.mapMaybe id

  logDebugN
    ("found these configurations with mrsk metadata: [ " <> T.intercalate ", " (HM.keys configurationMetadatas) <> " ]")

  derivations :: HashMap Text (Path Abs File) <-
    logExceptions
      =<< traverseThrottled
        4
        ( \name _ -> runConfigurationLogging name do
            tellEvaluationStarted name
            logDebugN "starting evaluation"

            let attr = "nixosConfigurations.\"" <> name <> "\".config.system.build.toplevel.drvPath"
            result <-
              (nixEval @Text . NixFlake $ flake <> "#" <> attr)
                >>= parseAbsFile . T.unpack

            logDebugN "finished evaluation" *> pure result
        )
        configurationMetadatas

  builtPaths :: HashMap Text (Path Abs Dir) <-
    logExceptions
      =<< traverseThrottled
        4
        ( \name derivation ->
            runConfigurationLogging name do
              logDebugN "starting build"
              tellBuildStarted name

              result <-
                ( nixBuild (NixDerivation derivation)
                    <&> fromLeft undefined . head
                )

              logDebugN "finished build" *> pure result
        )
        derivations

  _ :: HashMap Text (ExitCode, Text) <-
    logExceptions
      =<< traverseThrottled
        4
        ( \name NixOSConfiguration{remoteUser, targetHost} -> runConfigurationLogging name do
            tellDeploymentStarted name
            logDebugN "starting deploy"

            handle <- openConnection name remoteUser targetHost

            logDebugN "Remote connection opened"

            (exitCode, output) <-
              near handle $
                Near'SwitchNixos
                  { configuration = (builtPaths HM.! name)
                  , action
                  , sudo
                  }

            tellDeploymentFinished name exitCode output
            logInfoN (name <> " deployed with " <> T.show exitCode <> " and " <> output)

            pure (exitCode, output)
        )
        configurationMetadatas

  pure ()

runSomeLogging
  :: ( FileSystem :> es
     , IOE :> es
     , RequireCallStack
     , TypedProcess :> es
     )
  => Logging -> Eff (Logger : es) a -> Eff es a
runSomeLogging Logging'OtherTerminal = runOtherTerminalLogging
runSomeLogging (Logging'File path) = runFileLogging path
runSomeLogging Logging'Blackhole = runVoidLogging
runSomeLogging Logging'Stderr = runStderrLogging

main :: IO ()
main =
  provideCallStack $
    liftIO (execParser options) >>= \opts@Options{logging, nom, command} ->
      case command of
        Far ->
          runEff
            . runFailIO
            . runPrim
            . runFileSystem
            . runTypedProcess
            . runSomeLogging logging
            . runEnvironment
            . runConcurrent
            $ serveFar `catch` \(exception :: SomeException) ->
              logErrorN ("Far side caught exception: " <> T.show exception)
        _ -> effectStack logging nom $ (mrsk opts >> askConfirmExit Nothing) `catch` (askConfirmExit . Just)
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
      . runSomeLogging logging
      . runCliEffect Cli.Config{nom}
      . runNixEffect
      . runEnvironment
      . runRemote
