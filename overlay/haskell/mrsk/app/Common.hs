module Common where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.FileSystem.IO (BufferMode (LineBuffering), FileSystem, Handle, hSetBuffering)
import Effectful.Process.Typed (TypedProcess, proc, withProcessTerm)
import System.Posix.IO (closeFd, createPipe, fdToHandle)

withChildReader
  :: (FileSystem :> es, IOE :> es, TypedProcess :> es)
  => Text -> Text -> (Handle -> Eff es a) -> Eff es a
withChildReader terminal process act = do
  (outFd, inFd) <- liftIO $ createPipe
  inHandle <- liftIO $ fdToHandle inFd
  hSetBuffering inHandle LineBuffering

  withProcessTerm (proc (T.unpack terminal) ["-e", "sh", "-c", T.unpack process <> " <&" <> show outFd]) \_ -> do
    liftIO $ closeFd outFd

    act inHandle
