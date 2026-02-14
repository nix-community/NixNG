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
  tellNixInternalLog,
 )
import Cli qualified
import Control.Concurrent qualified as IO
import Control.Exception (SomeException (..))
import Control.Monad (foldM, forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Either (fromLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (AsyncCancelled (AsyncCancelled), Concurrently (..), forConcurrently, link, withAsync)
import Effectful.Concurrent.Chan (Chan, readChan)
import Effectful.Concurrent.MVar (putMVar)
import Effectful.Concurrent.MVar.Strict (MVar')
import Effectful.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Effectful.Dispatch.Dynamic (localUnlift)
import Effectful.Dispatch.Static (unEff, unsafeEff)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Exception (
  Exception (fromException),
  ExceptionWithContext,
  bracket_,
  catch,
  catchIf,
  handle,
  onException,
  throwIO,
 )
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO (Handle, stdin)
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
import Options (Command (..), Logging (..), Options (Options, command, dumpLog, logging, nom), options)
import Options.Applicative (execParser)
import Path.Posix (Abs, Dir, File, Path, parseAbsFile, toFilePath)
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
  , CliEffect
  , Nix
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
mrsk Options{command = Far} = error ""
mrsk Options{command = Replay{nixInternalLog, rate}} = do
  lines' <- case nixInternalLog of
    Just nixInternalLog' -> liftIO (BSL.readFile (toFilePath nixInternalLog')) <&> BSL.lines
    Nothing -> liftIO (BSL.hGetContents stdin) <&> BSL.lines

  forM_ lines' \line -> delay <* tellNixInternalLog (BS.toStrict line)
 where
  second :: Float = 1.0
  delay = case rate of
    Just rate' -> threadDelay . floor $ second / (fromIntegral rate') * 1_000_000
    Nothing -> pure ()
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
main = do
  queue :: IO.MVar (Chan ByteString) <- IO.newEmptyMVar

  provideCallStack $
    liftIO (execParser options) >>= \opts@Options{logging, nom, command, dumpLog} ->
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
        _ ->
          effectStack queue logging dumpLog nom $
            catchIf
              (\exc -> case fromException exc of Just AsyncCancelled -> False; _ -> True)
              (mrsk opts >> askConfirmExit Nothing)
              (askConfirmExit . Just)
 where
  tailer :: IO.MVar (Chan ByteString) -> Handle -> IO ()
  tailer queue h = do
    queue' <- IO.readMVar queue
    BSL.hGetContents h >>= mapM_ (IO.writeChan queue' . BSL.toStrict) . BSL.lines

  effectStack
    :: (RequireCallStack)
    => IO.MVar (Chan ByteString)
    -> Logging
    -> Maybe (Path Abs File)
    -> Bool
    -> Eff TopLevelEffects ()
    -> IO ()
  effectStack queue logging dumpLog nom eff = do
    queue' <- IO.newChan
    IO.putMVar queue queue'

    runEff
      . runPrim
      . runFailIO
      . runConcurrent
      . runFileSystem
      . runTypedProcess
      . runSomeLogging logging
      . runNixEffect (tailer queue)
      . runCliEffect Cli.Config{nom, dumpLog}
      . runEnvironment
      . runRemote
      $ withAsync (forever (readChan queue' >>= tellNixInternalLog)) \a -> link a >> eff
