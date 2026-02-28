{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Nix where

import Control.Monad (forM, forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types (FromJSON)
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BSL hiding (hGetContents)
import Data.Function ((&))
import Data.Functor (void)
import Data.HashSet qualified as HS
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, Subset, UnliftStrategy (..), inject, (:>))
import Effectful.Concurrent
import Effectful.Concurrent.Async (Async, async, asyncThreadId, cancelMany, cancelWith, mapConcurrently, withAsync)
import Effectful.Concurrent.MVar (readMVar)
import Effectful.Concurrent.MVar.Strict (modifyMVar', modifyMVar'_, newMVar', readMVar', takeMVar')
import Effectful.Dispatch.Dynamic (interpret, localUnlift, send)
import Effectful.Exception (Exception, IOException, catch, catchIf, finally, handle, mask, mask_)
import Effectful.FileSystem.IO (FileSystem, Handle)
import Effectful.FileSystem.IO.ByteString qualified as BS
import Effectful.FileSystem.IO.ByteString.Lazy qualified as BSL
import Effectful.Monad.Logger (Logger, logDebugN)
import GHC.IO.Exception (ioe_handle)
import GHC.Stack.Types (HasCallStack)
import Lens.Micro.Platform ((%~), _2)
import Nix.Derivation (Derivation)
import Nix.Select (NixTarget, Selector)
import Nix.Select qualified as Select
import Path.Posix (Abs, Dir, File, Path)
import RequireCallStack (RequireCallStack)
import System.IO.Error (isEOFError)
import System.NixNG.SomePath (SomePath)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
infixr 8 .:

data Nix :: Effect where
  NixBuild :: Text -> NixTarget Select.Build -> Nix m [Either (Path Abs Dir) (Path Abs File)]
  NixEval :: (FromJSON a) => Text -> NixTarget Select.Eval -> Nix m a
  NixCopy :: Text -> SomePath Abs -> Text -> Text -> Nix m ()
  NixDerivationShow :: Path Abs File -> Nix m (Derivation Text Text)
  NixSelect :: (FromJSON a) => Text -> [([Selector], A.Value -> A.Value)] -> Text -> Nix (Eff es) [a]
type instance DispatchOf Nix = Dynamic

nixBuild
  :: (HasCallStack, Nix :> es) => Text -> NixTarget Select.Build -> Eff es [Either (Path Abs Dir) (Path Abs File)]
nixBuild machine target = send (NixBuild machine target)

nixEval :: forall a es. (FromJSON a, HasCallStack, Nix :> es) => Text -> NixTarget Select.Eval -> Eff es a
nixEval machine target = send (NixEval machine target)

nixCopy :: forall es. (HasCallStack, Nix :> es) => Text -> SomePath Abs -> Text -> Text -> Eff es ()
nixCopy machine storePath remoteUser targetHost = send (NixCopy machine storePath remoteUser targetHost)

nixDerivationShow :: (HasCallStack, Nix :> es) => Path Abs File -> Eff es (Derivation Text Text)
nixDerivationShow derivation = send (NixDerivationShow derivation)

nixSelect :: (FromJSON a, HasCallStack, Nix :> es) => Text -> [([Selector], A.Value -> A.Value)] -> Text -> Eff es [a]
nixSelect machine selectors flakeRef = send (NixSelect machine selectors flakeRef)

data HandlesUpdated = HandlesUpdated
  deriving (Show)
instance Exception HandlesUpdated

runNixEffect
  :: forall es a
   . (Concurrent :> es, FileSystem :> es, IOE :> es, Logger :> es, RequireCallStack)
  => (Text -> Handle -> IO ()) -> Eff (Nix : es) a -> Eff es a
runNixEffect tailer eff = do
  readers <- newMVar' HS.empty

  let
    catchEOF :: Eff es () -> (IOException -> Eff es ()) -> Eff es ()
    catchEOF = catchIf isEOFError

    addHandle machine stderr = mask_ do
      a <- async (liftIO (tailer machine stderr) `catchEOF` const (pure ()))
      modifyMVar'_ readers (pure . HS.insert a)

  ( eff & interpret \_ val -> case val of
      NixBuild machine target -> Select.nixBuild target (addHandle machine)
      NixEval machine target -> Select.nixEval target (addHandle machine)
      NixCopy machine storePath remoteUser targetHost -> Select.nixCopy storePath remoteUser targetHost (addHandle machine)
      NixDerivationShow derivation -> Select.nixDerivationShow derivation
      NixSelect machine selectors flakeRef -> Select.select selectors flakeRef (addHandle machine)
    )
    `finally` do
      readers' <- takeMVar' readers
      (cancelMany (HS.toList readers'))
