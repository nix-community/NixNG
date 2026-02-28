{-# LANGUAGE FunctionalDependencies #-}

module Cli.Monitor where

import Brick.Types (Widget)
import Brick.Widgets.Core (txt, vBox, (<=>))
import Control.Applicative (Alternative ((<|>)))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Vector.Strict (Vector)
import Data.Vector.Strict qualified as V
import Data.Word (Word64)
import Effectful (Eff, (:>))
import Lens.Micro.Platform (makeLensesWith, (^.))
import Nix (Nix, nixDerivationShow)
import Nix.Derivation (Derivation (..))
import NixMessage (ActivityId, DerivationPath (..))
import Path.Posix (toFilePath)
import System.NixNG.TH (duplicateRules)

data Build = Build {derivation :: Derivation Text Text, name :: Text}
  deriving (Show)

data Monitor = Monitor
  { runningBuilds :: HashMap ActivityId Build
  , completedBuilds :: HashMap ActivityId Build
  , waitingBuilds :: HashMap ActivityId Build
  }
  deriving (Show)

makeLensesWith duplicateRules ''Build
makeLensesWith duplicateRules ''Monitor

emptyMonitor :: Monitor
emptyMonitor =
  Monitor
    { runningBuilds = HM.empty
    , completedBuilds = HM.empty
    , waitingBuilds = HM.empty
    }

guessNameFromDerivation :: DerivationPath -> Derivation Text Text -> Maybe Text
guessNameFromDerivation (DerivationPath derivationPath) Derivation{env} =
  case env M.!? "pname" <|> env M.!? "name" of
    Just name -> Just name
    Nothing ->
      case T.splitOn "/" . T.pack $ toFilePath derivationPath of
        _ : _ : nameWithHash : [] ->
          case T.splitOn "-" nameWithHash of
            _hash : name@(_ : _) -> Just $ T.intercalate "-" name
            _ -> Nothing
        _ -> Nothing

monitorStartedBuild :: (Nix :> es) => ActivityId -> DerivationPath -> Monitor -> Eff es Monitor
monitorStartedBuild id' (DerivationPath derivationPath) monitor = do
  derivation <- nixDerivationShow derivationPath

  let
    build =
      Build
        { derivation
        , name = maybe "???" id (guessNameFromDerivation (DerivationPath derivationPath) derivation)
        }

  pure $
    monitor
      { runningBuilds = HM.insert id' build (monitor ^. _runningBuilds)
      }

monitorCompletedBuild :: Monitor -> ActivityId -> Monitor
monitorCompletedBuild monitor id' =
  case (monitor ^. _runningBuilds) HM.!? id' of
    Just build ->
      monitor
        { runningBuilds = HM.delete id' (monitor ^. _runningBuilds)
        , completedBuilds = HM.insert id' build (monitor ^. _completedBuilds)
        }
    Nothing -> monitor

monitorWaitingBuild :: (Nix :> es) => ActivityId -> DerivationPath -> Monitor -> Eff es Monitor
monitorWaitingBuild id' (DerivationPath derivationPath) monitor = do
  derivation <- nixDerivationShow derivationPath

  let
    build =
      Build
        { derivation
        , name = maybe "???" id (guessNameFromDerivation (DerivationPath derivationPath) derivation)
        }

  pure $
    monitor
      { waitingBuilds = HM.insert id' build (monitor ^. _waitingBuilds)
      }

renderMonitor :: Monitor -> Widget n
renderMonitor monitor =
  txt ("┗━ ∑ ⏵ " <> runningBuilds' <> " │ ✔ " <> completedBuilds' <> " │ ⏸ " <> waitingBuilds' <> " │ ⏱ 46s")
 where
  runningBuilds' = T.show . HM.size $ monitor ^. _runningBuilds
  completedBuilds' = T.show . HM.size $ monitor ^. _completedBuilds
  waitingBuilds' = T.show . HM.size $ monitor ^. _waitingBuilds
