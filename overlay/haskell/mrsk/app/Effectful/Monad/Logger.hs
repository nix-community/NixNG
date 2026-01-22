{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Monad.Logger where

import Control.Monad.Logger (LoggingT (..), monadLoggerLog, toLogStr)
import Control.Monad.Logger qualified as LM
import Data.Text (Text)
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Effectful.FileSystem.IO
import Effectful.Process.Typed (TypedProcess, proc, setCloseFds, startProcess, withProcessTerm)
import Path.Posix (Abs, File, Path, toFilePath)
import RequireCallStack (RequireCallStack)
import System.Posix.IO (closeFd, createPipe, fdToHandle)

newtype Logging m = Logging (forall a. LoggingT m a -> IO a)

data Logger :: Effect

type instance DispatchOf Logger = Static WithSideEffects
newtype instance StaticRep Logger = Logger (Logging IO)

runStderrLogging :: (IOE :> es, RequireCallStack) => Eff (Logger : es) a -> Eff es a
runStderrLogging = evalStaticRep (Logger logger)
 where
  logger = Logging \val -> LM.runStderrLoggingT val

runFileLogging :: (IOE :> es, RequireCallStack) => Path Abs File -> Eff (Logger : es) a -> Eff es a
runFileLogging file = evalStaticRep (Logger foo)
 where
  foo = Logging \val -> LM.runFileLoggingT (toFilePath file) val

runVoidLogging :: (IOE :> es, RequireCallStack) => Eff (Logger : es) a -> Eff es a
runVoidLogging = evalStaticRep (Logger foo)
 where
  foo = Logging \LoggingT{runLoggingT} -> runLoggingT \_ _ _ _ -> pure ()

runOtherTerminalLogging
  :: (FileSystem :> es, IOE :> es, RequireCallStack, TypedProcess :> es) => Eff (Logger : es) a -> Eff es a
runOtherTerminalLogging eff = do
  (outFd, inFd) <- liftIO $ createPipe
  inHandle <- liftIO $ fdToHandle inFd
  hSetBuffering inHandle LineBuffering

  withProcessTerm (proc "alacritty" ["-e", "sh", "-c", "cat <&" <> show outFd]) \_ -> do
    liftIO $ closeFd outFd

    let logger = Logging (`LM.runLoggingT` LM.defaultOutput inHandle)

    evalStaticRep (Logger logger) eff

runConfigurationLogging
  :: (IOE :> es, Logger :> es) => Text -> Eff (Logger : es) a -> Eff es a
runConfigurationLogging configuration eff = do
  Logger (Logging log) <- getStaticRep

  evalStaticRep
    ( Logger $
        Logging @IO $
          ( `runLoggingT`
              \loc logSource logLevel logStr -> log (monadLoggerLog loc logSource logLevel ("[" <> toLogStr configuration <> "] " <> logStr))
          )
    )
    eff

logInfoN :: (Logger :> es, RequireCallStack) => Text -> Eff es ()
logInfoN msg = do
  Logger (Logging run) <- getStaticRep
  unsafeEff_ $ run (LM.logInfoN msg)

logDebugN :: (Logger :> es, RequireCallStack) => Text -> Eff es ()
logDebugN msg = do
  Logger (Logging run) <- getStaticRep
  unsafeEff_ $ run (LM.logDebugN msg)

logWarnN :: (Logger :> es, RequireCallStack) => Text -> Eff es ()
logWarnN msg = do
  Logger (Logging run) <- getStaticRep
  unsafeEff_ $ run (LM.logDebugN msg)

logErrorN :: (Logger :> es, RequireCallStack) => Text -> Eff es ()
logErrorN msg = do
  Logger (Logging run) <- getStaticRep
  unsafeEff_ $ run (LM.logErrorN msg)

instance (IOE :> es, Logger :> es) => LM.MonadLogger (Eff es) where
  monadLoggerLog loc logSource level msg =
    getStaticRep >>= \(Logger (Logging run)) -> liftIO . run $ LM.monadLoggerLog loc logSource level msg

instance (IOE :> es, LM.MonadLogger (Eff es), Logger :> es) => LM.MonadLoggerIO (Eff es) where
  askLoggerIO = getStaticRep >>= \(Logger (Logging run)) -> liftIO . run $ LM.LoggingT return
