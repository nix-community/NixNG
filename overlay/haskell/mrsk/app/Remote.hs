{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Remote where

import Cli (CliEffect, askReadLine)
import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad (forever)
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, monadLoggerLog)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, hoistMaybe, runMaybeT)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.Types (FromJSON, JSONPathElement (Key), parseJSON, toJSON, (.:), (.=), (<?>))
import Data.Aeson.Types qualified as A
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Debug.Trace (trace)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, IOE, inject, subsume, (:>))
import Effectful.Concurrent (Concurrent, ThreadId, forkIO, threadDelay)
import Effectful.Concurrent.Async (Async, async, cancel, race)
import Effectful.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Effectful.Concurrent.MVar.Strict (MVar', newEmptyMVar', putMVar', takeMVar')
import Effectful.Dispatch.Dynamic (interpret, reinterpret, send)
import Effectful.Environment (Environment, lookupEnv)
import Effectful.Exception (throwIO)
import Effectful.Monad.Logger (Logger, logDebugN, logErrorN, runConfigurationLogging)
import Effectful.Prim.IORef.Strict (Prim, newIORef', readIORef', writeIORef')
import Effectful.Process.Typed (
  ExitCode (..),
  Process,
  ProcessConfig,
  TypedProcess,
  createPipe,
  getExitCode,
  getStderr,
  getStdin,
  getStdout,
  proc,
  readProcessInterleaved,
  runProcess,
  setStderr,
  setStdin,
  setStdout,
  startProcess,
 )
import Effectful.State.Dynamic (evalStateLocal, evalStateShared, gets, modify, state)
import GHC.Generics (Generic)
import Lens.Micro.Platform (Lens', at, each, makeLensesWith, mapped, use, (%=), (+=), (^..))
import Orphans ()
import Path.Posix (Abs, Dir, File, Path, parseAbsFile, toFilePath)
import Remote.Common (
  Local (..),
  Message (..),
  Message',
  RemoteException (ConnectionFailed, CouldNotLocateMrskBinary, address, output, user),
  SomeMessage (..),
  SomeResponse (..),
  acquireId,
  background,
  newProtocol,
  protoRead,
  protoRespond,
  protoWrite,
 )
import RequireCallStack (RequireCallStack)
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.IO qualified as IO
import System.NixNG.TH (duplicateRules)

data Far :: Message' where
  Far'AskSudoPassword :: Far Text
  Far'Log :: Loc -> LogSource -> LogLevel -> LogStr -> Far ()

instance A.FromJSON (Far Text) where
  parseJSON = A.withObject "Far" \obj ->
    obj .: "type" >>= \case
      ("Far'AskSudoPassword" :: Text) -> pure Far'AskSudoPassword
      _ -> fail "unknown type"

instance A.FromJSON (Far ()) where
  parseJSON = A.withObject "Far" \obj ->
    obj .: "type" >>= \case
      ("Far'Log" :: Text) ->
        Far'Log
          <$> obj .: "loc"
          <*> obj .: "logSource"
          <*> obj .: "logLevel"
          <*> obj .: "logStr"
      _ -> fail "unknown type"

instance A.ToJSON (Far Text) where
  toJSON Far'AskSudoPassword = A.object ["type" .= A.String "Far'AskSudoPassword"]
instance A.ToJSON (Far ()) where
  toJSON (Far'Log loc logSource logLevel logStr) =
    A.object
      [ "type" .= A.String "Far'Log"
      , "loc" .= loc
      , "logSource" .= logSource
      , "logLevel" .= logLevel
      , "logStr" .= logStr
      ]
instance FromJSON (SomeMessage Far) where
  parseJSON val = (parseJSON @(Far Text) val <&> SomeMessage) <|> (parseJSON @(Far ()) val <&> SomeMessage)

data SwitchAction
  = SwitchAction'Check
  | SwitchAction'Switch
  | SwitchAction'Boot
  | SwitchAction'Test
  | SwitchAction'DryActivate
  deriving stock (Generic, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

data Near :: Message' where
  Near'ExecuteCommand
    :: { program :: Text
       , sudo :: Bool
       , arguments :: [Text]
       }
    -> Near (ExitCode, Text)
  Near'SwitchNixos
    :: { configuration :: Path Abs Dir
       , action :: SwitchAction
       , sudo :: Bool
       }
    -> Near (ExitCode, Text)

instance A.FromJSON (Near (ExitCode, Text)) where
  parseJSON = A.withObject "Near" \obj ->
    obj .: "type" <?> Key "type" >>= \case
      ("Near'ExecuteCommand" :: Text) ->
        Near'ExecuteCommand
          <$> obj .: "program" <?> Key "program"
          <*> obj .: "sudo" <?> Key "sudo"
          <*> obj .: "arguments" <?> Key "sudo"
      "Near'SwitchNixos" ->
        Near'SwitchNixos
          <$> obj .: "configuration" <?> Key "configuration"
          <*> obj .: "action" <?> Key "action"
          <*> obj .: "sudo" <?> Key "sudo"
      _ -> fail "unknown type"
instance A.ToJSON (Near (ExitCode, Text)) where
  toJSON (Near'ExecuteCommand{program, sudo, arguments}) =
    A.object
      [ "program" .= program
      , "sudo" .= sudo
      , "arguments" .= arguments
      , "type" .= ("Near'ExecuteCommand" :: Text)
      ]
  toJSON (Near'SwitchNixos{configuration, action, sudo}) =
    A.object
      [ "configuration" .= configuration
      , "action" .= action
      , "sudo" .= sudo
      , "type" .= ("Near'SwitchNixos" :: Text)
      ]

instance FromJSON (SomeMessage Near) where
  parseJSON val = parseJSON @(Near (ExitCode, Text)) val <&> SomeMessage

newtype Handle = Handle Int

data RemoteConnection
  = RemoteConnection
  { process :: Process IO.Handle IO.Handle IO.Handle
  , channel :: Chan (Local Near)
  , a :: Async ()
  }

data RemoteEffectState = RemoteEffectState
  { counter :: Int
  , connections :: HashMap Int RemoteConnection
  }

data ConnectionException = ConnectionException
  deriving (Show)
instance Exception ConnectionException

data InvalidIdException = InvalidIdException {id :: Int}
  deriving (Show)
instance Exception InvalidIdException

makeLensesWith duplicateRules ''Message
makeLensesWith duplicateRules ''RemoteEffectState
makeLensesWith duplicateRules ''RemoteConnection

data RemoteEffect m a where
  OpenConnection :: {configuration :: Text, user :: Text, address :: Text} -> RemoteEffect m Handle
  Near :: (FromJSON a, ToJSON (Near a)) => {connection :: Handle, message :: Near a} -> RemoteEffect m a
type instance DispatchOf RemoteEffect = Dynamic

openConnection :: (RemoteEffect :> es, RequireCallStack) => Text -> Text -> Text -> Eff es Handle
openConnection configuration user address = send (OpenConnection{configuration, user, address})

near
  :: forall es a. (FromJSON a, RemoteEffect :> es, RequireCallStack, ToJSON (Near a)) => Handle -> Near a -> Eff es a
near connection message =
  send (Near{connection, message})

remoteProc :: Text -> Text -> Text -> [Text] -> ProcessConfig () () ()
remoteProc user address program arguments =
  proc "ssh" $ map T.unpack arguments'
 where
  arguments' =
    [ "-l" -- login
    , user
    , "-p" -- port
    , "22"
    , "-e" -- escape char
    , "none"
    , "-q" -- quiet
    , address
    , "--"
    , program
    ]
      <> arguments

remoteCopy :: Text -> Text -> Text -> Text -> ProcessConfig () () ()
remoteCopy user address source destination =
  proc "scp" . map T.unpack $
    [ "-P" -- port
    , "22"
    , "-q" -- quiet
    , "--"
    , source
    , user <> "@" <> address <> ":" <> destination
    ]

locateMrskBinary
  :: forall es
   . (Environment :> es, Logger :> es, RequireCallStack, TypedProcess :> es)
  => Text -> Text -> Eff es (Maybe (Path Abs File))
locateMrskBinary user address = runMaybeT do
  lift (runProcess $ remoteProc user address "true" []) >>= \case
    ExitSuccess -> pure ()
    ExitFailure _ -> hoistMaybe Nothing

  homeDirectory <-
    lift (readProcessInterleaved $ remoteProc user address "getent" ["passwd", user]) >>= \case
      (ExitSuccess, output) ->
        case BSL.split (fromIntegral $ ord ':') output of
          [_, _, _, _, _, home, _] -> pure (T.decodeUtf8 . BS.toStrict $ home)
          r -> do
            lift . logDebugN . T.show $ r
            lift . logDebugN $ "could not get home directory"
            hoistMaybe Nothing
      (ExitFailure _, _) -> hoistMaybe Nothing

  let
    bsToText = T.decodeUtf8 . BS.toStrict

    tryPath =
      lift (readProcessInterleaved $ remoteProc user address "command" ["-v", "mrsk"]) >>= \case
        (ExitSuccess, stdout) -> do
          lift $ logDebugN $ "Found mrsk binary in path"
          parseAbsFile (T.unpack $ bsToText stdout) >>= hoistMaybe . Just
        (ExitFailure _, _) -> do
          lift $ logDebugN $ "No remote mrsk binary"
          hoistMaybe Nothing

    localFromCabal :: MaybeT (Eff es) Text
    localFromCabal =
      lift (readProcessInterleaved $ proc "cabal" ["list-bin", "mrsk"]) >>= \case
        (ExitSuccess, stdout) -> do
          lift $ logDebugN $ "Found mrsk binary locally through cabal"
          hoistMaybe . Just . T.strip $ bsToText stdout
        (ExitFailure _, _) -> do
          lift $ logDebugN $ "No mrsk binary found through cabal"
          hoistMaybe Nothing

    localFromEnv :: MaybeT (Eff es) Text
    localFromEnv =
      lift (lookupEnv "MRSK_BINARY") >>= \case
        Just value -> do
          lift $ logDebugN $ "Found mrsk binary through environment variable"
          hoistMaybe . Just $ T.pack value
        Nothing -> do
          lift $ logDebugN $ "no mrsk binary in environment variable"
          hoistMaybe Nothing

    copyLocal = do
      self <- localFromCabal <|> localFromEnv
      let remoteSelf = (homeDirectory <> "/.cache/mrsk")

      lift (runProcess $ remoteProc user address "mkdir" ["-p", homeDirectory <> "/.cache"]) >>= \case
        ExitSuccess -> do
          lift . logDebugN $ "Created remote ~/.cache directory"
          pure ()
        ExitFailure _ -> do
          lift . logDebugN $ "Failed to remote ~/.cache directory"
          hoistMaybe Nothing

      lift (readProcessInterleaved $ remoteCopy user address self remoteSelf) >>= \case
        (ExitSuccess, _) -> do
          lift . logDebugN $ "Successfully copied mrsk to ~/.cache/mrsk"
          pure ()
        (ExitFailure exitCode, output) -> do
          lift . logDebugN $ "Failed to copy mrsk to ~/.cache/mrsk exit code: " <> T.show exitCode <> " output: " <> T.show output
          hoistMaybe Nothing

      parseAbsFile $ T.unpack remoteSelf

  copyLocal <|> tryPath

runRemote
  :: ( CliEffect :> es
     , Concurrent :> es
     , Environment :> es
     , IOE :> es
     , Logger :> es
     , Prim :> es
     , RequireCallStack
     , TypedProcess :> es
     )
  => Eff (RemoteEffect : es) a -> Eff es a
runRemote eff = evalStateLocal initialState do
  result <-
    (inject eff) & interpret \_ -> \case
      OpenConnection{configuration, user, address} -> runConfigurationLogging configuration do
        logDebugN ("opening connection to " <> user <> "@" <> address)

        handle <- use _counter'
        _counter' += 1

        mrskBinary <-
          locateMrskBinary user address >>= \case
            Just mrskBinary -> pure mrskBinary
            Nothing -> throwIO CouldNotLocateMrskBinary{user, address}

        process <-
          startProcess . setStdin createPipe . setStdout createPipe . setStderr createPipe $
            remoteProc user address (T.pack . toFilePath $ mrskBinary) ["far", "--logging", "blackhole"]

        -- wait a 100 miliseconds for the command to catch on or exit
        threadDelay (1_000_00)

        getExitCode process >>= \case
          Just _ -> do
            stdout <- liftIO $ BSL.hGetContents (getStdout process)
            stderr <- liftIO $ BSL.hGetContents (getStderr process)
            throwIO ConnectionFailed{user, address, output = stdout <> stderr}
          Nothing -> pure ()

        mapM_
          (liftIO . (`hSetBuffering` LineBuffering))
          [ getStdout process -- getStderr process,
          , getStdin process
          ]

        (channel, a) <- background (getStdin process) (getStdout process) \_channel -> \case
          Far'AskSudoPassword -> do
            askReadLine ("sudo password for " <> configuration <> ": ") <&> A.toJSON
          Far'Log loc logSource logLevel logStr -> do
            monadLoggerLog loc logSource logLevel logStr
            pure $ A.toJSON ()

        _connections' %= HM.insert handle RemoteConnection{process, channel, a}

        logDebugN ("connection to " <> user <> "@" <> address <> " opened")

        pure $ Handle handle
      Near{connection = Handle connection, message} ->
        use (_connections' . at connection) >>= \case
          Just (RemoteConnection{channel}) -> do
            result <- newEmptyMVar'
            writeChan channel $ Local'Message{result, message}

            takeMVar' result
          Nothing -> throwIO ConnectionException

  gets (^.. _connections' . each . _a) >>= mapM_ \a -> do
    cancel a

  pure result
 where
  initialState = RemoteEffectState{counter = 0, connections = HM.empty}

  _counter' :: Lens' RemoteEffectState Int
  _counter' = _counter
  _connections' :: Lens' RemoteEffectState (HashMap Int RemoteConnection)
  _connections' = _connections
