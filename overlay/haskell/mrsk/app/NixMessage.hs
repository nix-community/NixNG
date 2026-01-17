{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}

module NixMessage (
  maybeLevel,
  NixJSONMessage (..),
  StartAction (..),
  _id,
  _level,
  _text,
  _activity,
  StopAction (..),
  -- _id,
  _result,
  MessageAction (..),
  -- _level,
  _message,
  ResultAction (..),
  ActivityResult (..),
  Activity (..),
  ActivityId (..),
  Verbosity (..),
  ActivityProgress (..),
  _done,
  _running,
  _expected,
  _failed,
  ActivityType (..),
  parseDerivation,
  parseStorePath,
  parseHost,
) where

import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text (Text)
import Lens.Micro.Platform (Lens, lens, makeLensesWith, (%~), (.~), (^.))
import System.NixNG.TH (duplicateRules)

newtype Derivation = Derivation Text
  deriving stock (Eq, Ord, Show)

newtype StorePath = StorePath Text
  deriving stock (Eq, Ord, Show)

newtype Host = Host Text
  deriving stock (Eq, Ord, Show)

newtype ActivityId = MkId {value :: Int}
  deriving newtype (Eq, Ord, Show)

newtype StopAction = MkStopAction {id :: ActivityId}
  deriving stock (Show)
  deriving newtype (Eq)

data Verbosity where
  Error :: Verbosity
  Warn :: Verbosity
  Notice :: Verbosity
  Info :: Verbosity
  Talkative :: Verbosity
  Chatty :: Verbosity
  Debug :: Verbosity
  Vomit :: Verbosity
  deriving stock (Eq, Ord, Show)

data ActivityType where
  UnknownType :: ActivityType
  CopyPathType :: ActivityType
  FileTransferType :: ActivityType
  RealiseType :: ActivityType
  CopyPathsType :: ActivityType
  BuildsType :: ActivityType
  BuildType :: ActivityType
  OptimiseStoreType :: ActivityType
  VerifyPathsType :: ActivityType
  SubstituteType :: ActivityType
  QueryPathInfoType :: ActivityType
  PostBuildHookType :: ActivityType
  BuildWaitingType :: ActivityType
  FetchTreeType :: ActivityType
  deriving stock (Eq, Show)

-- | nix src/libutil/include/nix/util/logging.hh ActivityType
data Activity where
  Unknown :: Activity
  CopyPath :: StorePath -> Host {- WithContext -} -> Host {- WithContext -} -> Activity
  FileTransfer :: Text -> Activity
  Realise :: Activity
  CopyPaths :: Activity
  Builds :: Activity
  Build :: Derivation -> Host {- WithContext -} -> Activity
  OptimiseStore :: Activity
  VerifyPaths :: Activity
  Substitute :: StorePath -> Host {- WithContext -} -> Activity
  QueryPathInfo :: StorePath -> Host {- WithContext -} -> Activity
  PostBuildHook :: Derivation -> Activity
  BuildWaiting :: Activity
  FetchTree :: Activity
  deriving stock (Eq, Ord, Show)

-- | nix src/libutil/include/nix/util/logging.hh ResultType
data ActivityResult where
  FileLinked :: Int -> Int -> ActivityResult
  BuildLogLine :: Text -> ActivityResult
  UntrustedPath :: StorePath -> ActivityResult
  CorruptedPath :: StorePath -> ActivityResult
  SetPhase :: Text -> ActivityResult
  Progress :: ActivityProgress -> ActivityResult
  SetExpected :: ActivityType -> Int -> ActivityResult
  PostBuildLogLine :: Text -> ActivityResult
  FetchStatus :: Text -> ActivityResult
  deriving stock (Eq, Show)

data ActivityProgress = MkActivityProgress
  { done :: Int
  , expected :: Int
  , running :: Int
  , failed :: Int
  }
  deriving stock (Eq, Ord, Show)

data StartAction = MkStartAction
  { id :: ActivityId
  , level :: Verbosity
  , text :: Text
  , activity :: Activity
  }
  deriving stock (Eq, Show)

data ResultAction = MkResultAction
  { id :: ActivityId
  , result :: ActivityResult
  }
  deriving stock (Eq, Show)

data MessageAction = MkMessageAction
  { level :: Verbosity
  , message :: Text
  -- currently unused, but theoretically present in the protocol
  -- , line :: Maybe Int
  -- , column :: Maybe Int
  -- , file :: Maybe Text
  }
  deriving stock (Eq, Show)

data NixJSONMessage where
  Stop :: StopAction -> NixJSONMessage
  Start :: StartAction -> NixJSONMessage
  Result :: ResultAction -> NixJSONMessage
  Message :: MessageAction -> NixJSONMessage
  Plain :: ByteString -> NixJSONMessage
  ParseError :: SomeException -> NixJSONMessage
  deriving stock (Show)

makeLensesWith duplicateRules ''Verbosity
makeLensesWith duplicateRules ''ActivityType
makeLensesWith duplicateRules ''Activity
makeLensesWith duplicateRules ''ActivityResult
makeLensesWith duplicateRules ''ActivityProgress
makeLensesWith duplicateRules ''StartAction
makeLensesWith duplicateRules ''ResultAction
makeLensesWith duplicateRules ''MessageAction
makeLensesWith duplicateRules ''NixJSONMessage

maybeLevel (Stop _) = Nothing
maybeLevel (Start startAction) = Just $ startAction ^. _level
maybeLevel (Result _) = Nothing
maybeLevel (Message messageAction) = Just $ messageAction ^. _level
maybeLevel (Plain _) = Nothing
maybeLevel (ParseError _) = Nothing

parseDerivation :: (MonadFail m) => Text -> m Derivation
parseDerivation = pure . Derivation

parseStorePath :: (MonadFail m) => Text -> m StorePath
parseStorePath = pure . StorePath

parseHost :: Text -> Host
parseHost = Host
