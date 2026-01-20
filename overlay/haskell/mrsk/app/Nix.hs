{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Nix where

import Cli (CliEffect, tellNixInternalLog)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types (FromJSON)
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Function ((&))
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, UnliftStrategy (..), (:>))
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic (interpret, localUnlift, send)
import GHC.Stack.Types (HasCallStack)
import Lens.Micro.Platform ((%~), _2)
import Nix.Select (Derivation, NixTarget, Selector)
import Nix.Select qualified as Select
import Path.Posix (Abs, Dir, File, Path)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
infixr 8 .:

data Nix :: Effect where
  NixBuild :: NixTarget Select.Build -> Nix m [Either (Path Abs Dir) (Path Abs File)]
  NixEval :: (FromJSON a) => NixTarget Select.Eval -> Nix m a
  NixDerivationShow :: Path Abs File -> Nix m Derivation
  NixSelect :: (FromJSON a) => [([Selector], A.Value -> A.Value)] -> Text -> Nix (Eff es) [a]
type instance DispatchOf Nix = Dynamic

nixBuild :: (HasCallStack, Nix :> es) => NixTarget Select.Build -> Eff es [Either (Path Abs Dir) (Path Abs File)]
nixBuild target = send (NixBuild target)

nixEval :: forall a es. (FromJSON a, HasCallStack, Nix :> es) => NixTarget Select.Eval -> Eff es a
nixEval target = send (NixEval target)

nixDerivationShow :: (HasCallStack, Nix :> es) => Path Abs File -> Eff es Derivation
nixDerivationShow derivation = send (NixDerivationShow derivation)

nixSelect :: (FromJSON a, HasCallStack, Nix :> es) => [([Selector], A.Value -> A.Value)] -> Text -> Eff es [a]
nixSelect selectors flakeRef = send (NixSelect selectors flakeRef)

runNixEffect :: (CliEffect :> es, Concurrent :> es, HasCallStack, IOE :> es) => Eff (Nix : es) a -> Eff es a
runNixEffect eff = do
  let logFn = \stderr -> void . forkIO $ do
        liftIO (BSL.hGetContents stderr) >>= mapM_ tellNixInternalLog . BSL.lines
  eff & interpret \_ val -> case val of
    NixBuild target -> Select.nixBuild target logFn
    NixEval target -> Select.nixEval target logFn
    NixDerivationShow derivation -> Select.nixDerivationShow derivation logFn
    NixSelect selectors flakeRef -> Select.select selectors flakeRef logFn
