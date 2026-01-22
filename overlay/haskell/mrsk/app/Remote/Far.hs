{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Remote.Far where

import Cli (CliEffect)
import Control.Monad (forever, when)
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT (..))
import Data.Aeson (toJSON)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as BS
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Effectful (Eff, IOE, UnliftStrategy (SeqForkUnlift), withEffToIO, (:>))
import Effectful.Concurrent.Async (wait)
import Effectful.Concurrent.Chan (Chan, Concurrent, writeChan)
import Effectful.Concurrent.MVar.Strict (newEmptyMVar', takeMVar')
import Effectful.Dispatch.Static (evalStaticRep, unsafeEff_)
import Effectful.Environment (Environment, getProgName)
import Effectful.Exception (Exception, throwIO)
import Effectful.Fail (Fail)
import Effectful.Monad.Logger (Logger, Logging (Logging), StaticRep (Logger), logDebugN, logErrorN, logInfoN)
import Effectful.Prim (Prim)
import Effectful.Prim.IORef (newIORef, readIORef)
import Effectful.Prim.IORef.Strict (newIORef', readIORef', writeIORef')
import Effectful.Process (CmdSpec (..))
import Effectful.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  ProcessConfig,
  TypedProcess,
  byteStringInput,
  byteStringOutput,
  nullStream,
  proc,
  readProcess,
  readProcessInterleaved,
  runProcess,
  runProcess_,
  setEnv,
  setStderr,
  setStdin,
  setStdout,
  shell,
 )
import Effectful.State.Dynamic (State, evalStateShared)
import Orphans ()
import Path.Posix (Abs, Dir, Path, absdir, relfile, toFilePath, (</>))
import Remote (
  Far (Far'AskSudoPassword, Far'Log),
  Near (Near'ExecuteCommand, Near'SwitchNixos, action, arguments, configuration, program, sudo),
  SwitchAction (..),
 )
import Remote.Common (
  Local (..),
  Message (Message, id', message),
  Protocol,
  SomeMessage (SomeMessage),
  SomeResponse (SomeResponse, id', response),
  acquireId,
  background,
  newProtocol,
  protoRead,
  protoRespond,
  protoWrite,
  protoWriteIO,
 )
import RequireCallStack (RequireCallStack, provideCallStack)
import System.Environment (executablePath)
import System.IO (hClose, stdin, stdout)
import System.NixNG.SomePath (SomePath (SomePath))
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Process.Typed.Internal (pcCmdSpec)
import Text.Read (readMaybe)

newtype Profile = Profile (Path Abs Dir)
  deriving newtype (Eq, Show)

data ProfileInfo = ProfileInfo {number :: Int, dateTime :: UTCTime, current :: Bool}
  deriving (Eq, Show)

type ProcessRunner stdin stdout stderr es a = ProcessConfig stdin stdout stderr -> Eff es a

data NixRemoteFarException
  = SetProfileException {exitCode :: ExitCode, output :: LazyByteString}
  | ListProfileException {exitCode :: ExitCode, output :: LazyByteString}
  deriving (Show)
instance Exception NixRemoteFarException

nixEnvListGenerations
  :: (Logger :> es, RequireCallStack, TypedProcess :> es)
  => Maybe (ProcessRunner () () () es (ExitCode, LazyByteString) -> ProcessRunner () () () es (ExitCode, LazyByteString))
  -> Profile
  -> Eff es [ProfileInfo]
nixEnvListGenerations run (Profile profile) = do
  (exitCode, output) <-
    maybe readProcessInterleaved ($ readProcessInterleaved) run $
      proc "nix-env" ["-p", toFilePath $ profile, "--list-generations"]

  case exitCode of
    ExitFailure _ -> do
      throwIO ListProfileException{exitCode, output}
    _ -> pure ()

  let
    parts =
      map (filter (not . TL.all (== ' ')) . TL.groupBy (\f s -> not $ (f == ' ') `xor` (s == ' '))) . TL.lines $
        TL.decodeUtf8 output

    buildProfileInfo number' year time current = do
      number <- readMaybe $ TL.unpack number'
      dateTime <- parseTimeM True defaultTimeLocale "%F %T" $ TL.unpack $ year <> " " <> time
      Just $
        ProfileInfo
          { number
          , dateTime
          , current
          }

    profileInfos = (`map` parts) \case
      [number', year, time, "(current)"] -> buildProfileInfo number' year time True
      [number', year, time] -> buildProfileInfo number' year time False
      _ -> Nothing

  unsafeEff_ $ print parts

  pure . catMaybes $ profileInfos

nixEnvSetProfile
  :: (Logger :> es, RequireCallStack, TypedProcess :> es)
  => Maybe (ProcessRunner () () () es (ExitCode, LazyByteString) -> ProcessRunner () () () es (ExitCode, LazyByteString))
  -> Profile
  -> SomePath Abs
  -> Eff es (ExitCode, Text)
nixEnvSetProfile run (Profile profile) (SomePath path) = do
  (exitCode, output) <-
    maybe readProcessInterleaved ($ readProcessInterleaved) run $
      proc "nix-env" ["--profile", toFilePath profile, "--set", toFilePath path]

  case exitCode of
    ExitFailure _ -> do
      throwIO $ SetProfileException{exitCode, output}
    ExitSuccess -> pure (exitCode, T.decodeUtf8 $ BS.toStrict output)

sudoProcess
  :: (Environment :> es, Fail :> es, IOE :> es, Logger :> es, RequireCallStack, TypedProcess :> es)
  => Eff es Text -> ProcessRunner stdin stdout stderr es a -> ProcessRunner stdin stdout stderr es a
sudoProcess password run pc = do
  let
    checkPc =
      proc "sudo" ["-Sv"]
        & setStdin nullStream
        & setStdout nullStream
        & setStderr nullStream

    pc' =
      case pcCmdSpec pc of
        RawCommand command args ->
          pc{pcCmdSpec = RawCommand "sudo" (["--non-interactive", "--", command] ++ args)}
        ShellCommand command ->
          pc{pcCmdSpec = ShellCommand ("sudo --non-interactive -- " <> command)}

  runProcess checkPc >>= \case
    ExitSuccess -> pure ()
    ExitFailure err -> do
      logDebugN $ "sudo -Sv exited with " <> T.show err
      sudoPassword <- password

      let
        truePc =
          proc "sudo" ["-Si", "true"]
            & setStdin (byteStringInput . BS.fromStrict $ T.encodeUtf8 (sudoPassword <> "\n"))
            & setStdout byteStringOutput
            & setStderr byteStringOutput
      runProcess_ truePc

  run pc'

serveFar
  :: ( CliEffect :> es
     , Concurrent :> es
     , Environment :> es
     , Fail :> es
     , IOE :> es
     , Logger :> es
     , Prim :> es
     , RequireCallStack
     , TypedProcess :> es
     )
  => Eff es ()
serveFar = do
  (channel :: Chan (Local Far), a) <- background stdout stdin \channel ->
    runFarLogging channel . \case
      Near'ExecuteCommand{program, sudo, arguments} -> do
        method <-
          if sudo
            then do
              pure $
                sudoProcess
                  ( do
                      logDebugN $ "[Far] asking for sudo password"

                      result <- newEmptyMVar'

                      writeChan channel $ Local'Message{result, message = Far'AskSudoPassword}

                      takeMVar' result
                  )
                  runProcess
            else
              pure runProcess

        exitCode <- method $ proc (T.unpack program) (map T.unpack arguments)

        pure $ toJSON (exitCode, "" :: Text)
      Near'SwitchNixos{configuration, action, sudo} -> do
        let
          sudoPassword = do
            logDebugN $ "[Far] asking for sudo password"

            result <- newEmptyMVar'

            writeChan channel $ Local'Message{result, message = Far'AskSudoPassword}

            takeMVar' result

        method <-
          if sudo
            then
              pure $
                sudoProcess sudoPassword readProcessInterleaved
            else
              pure readProcessInterleaved

        let
          setProfile =
            provideCallStack $
              nixEnvSetProfile
                (Just $ sudoProcess sudoPassword)
                (Profile [absdir|/nix/var/nix/profiles/system|])
                (SomePath configuration)
          switchToConfiguration actionText =
            method pc <&> \(exitCode, output) -> (exitCode, BS.decodeUtf8 $ BS.toStrict output)
           where
            pc = proc (toFilePath $ configuration </> [relfile|bin/switch-to-configuration|]) [actionText]

        (exitCode, output) <-
          case action of
            SwitchAction'Check -> do
              logInfoN "Performing a check"
              switchToConfiguration "check"
            SwitchAction'Switch -> do
              logInfoN "Performing a switch"
              _ <- setProfile
              switchToConfiguration "switch"
            SwitchAction'Boot -> do
              logInfoN "Performing a boot"
              setProfile
            SwitchAction'Test -> do
              logInfoN "Performing a boot"
              switchToConfiguration "test"
            SwitchAction'DryActivate -> do
              logDebugN $ "Performing a dry-activation"
              switchToConfiguration "dry-activate"

        pure $ toJSON (exitCode, output)

  wait a

foo
  :: (Concurrent :> es, IOE :> es, RequireCallStack)
  => Chan (Local Far) -> (forall a. Eff es a -> IO a) -> Logging IO
foo channel unlift =
  Logging (`runLoggingT` \loc logSource logLevel logStr -> unlift $ doLog channel loc logSource logLevel logStr)

doLog
  :: (Concurrent :> es, IOE :> es, RequireCallStack)
  => Chan (Local Far) -> Loc -> LogSource -> LogLevel -> LogStr -> Eff es ()
doLog channel loc logSource logLevel logStr = do
  result <- newEmptyMVar'
  writeChan channel $ Local'Message{result, message = Far'Log loc logSource logLevel logStr}

runFarLogging :: (Concurrent :> es, IOE :> es, RequireCallStack) => Chan (Local Far) -> Eff (Logger : es) a -> Eff es a
runFarLogging channel eff = withEffToIO SeqForkUnlift \unlift -> unlift $ evalStaticRep (Logger $ foo channel unlift) eff
