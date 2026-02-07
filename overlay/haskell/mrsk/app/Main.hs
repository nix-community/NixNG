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
  tellCopyStarted,
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
import Effectful.Concurrent.Async (Concurrently (..), forConcurrently)
import Effectful.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Exception (ExceptionWithContext, bracket_, catch, handle, throwIO)
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
import Nix (Nix, nixBuild, nixCopy, nixEval, nixSelect, runNixEffect)
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
import System.NixNG.SomePath (SomePath (SomePath))

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

  _ <- forConcurrently configurations \configuration ->
    runConfigurationLogging configuration $
      tellException configuration
        `handle` ( getNixOSConfigurationMetadata [configuration] flake <&> (HM.! configuration) >>= \case
                     Just NixOSConfiguration{remoteUser, targetHost} -> do
                       logDebugN "found metadata"

                       tellEvaluationStarted configuration
                       logDebugN "starting evaluation"

                       derivation <-
                         let attr = "nixosConfigurations.\"" <> configuration <> "\".config.system.build.toplevel.drvPath"
                          in (nixEval @Text . NixFlake $ flake <> "#" <> attr) >>= parseAbsFile . T.unpack

                       logDebugN "finished evaluation"

                       logDebugN "starting build"
                       tellBuildStarted configuration

                       storePath <- nixBuild (NixDerivation derivation) <&> fromLeft undefined . head
                       logDebugN "finished build"

                       tellCopyStarted configuration <* nixCopy (SomePath storePath) remoteUser targetHost

                       tellDeploymentStarted configuration
                       logDebugN "starting deploy"

                       remoteConnection <- openConnection configuration remoteUser targetHost

                       logDebugN "Remote connection opened"

                       (exitCode, output) <-
                         near remoteConnection $
                           Near'SwitchNixos
                             { configuration = storePath
                             , action
                             , sudo
                             }

                       tellDeploymentFinished configuration exitCode output
                       logInfoN (configuration <> " deployed with " <> T.show exitCode <> " and " <> output)
                     Nothing -> pure ()
                 )
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
