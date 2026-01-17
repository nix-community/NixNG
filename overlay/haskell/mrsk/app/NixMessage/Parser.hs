{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module NixMessage.Parser (parseJSONLine) where

import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.Text (Text)
import NixMessage (
  Activity (..),
  ActivityId (..),
  ActivityProgress (..),
  ActivityResult (..),
  ActivityType (..),
  MessageAction (..),
  NixJSONMessage (..),
  ResultAction (..),
  StartAction (..),
  StopAction (..),
  Verbosity (..),
  parseDerivation,
  parseHost,
  parseStorePath,
 )

parseJSONLine :: LazyByteString -> Either String NixJSONMessage
parseJSONLine input = maybe undefined Right (BSL.stripPrefix "@nix " input) >>= A.eitherDecode

instance A.FromJSON NixJSONMessage where
  parseJSON :: A.Value -> A.Parser NixJSONMessage
  parseJSON = parseAction

parseVerbosity :: A.Value -> A.Parser Verbosity
parseVerbosity = A.withScientific "Verbosity level" \case
  0 -> pure Error
  1 -> pure Warn
  2 -> pure Notice
  3 -> pure Info
  4 -> pure Talkative
  5 -> pure Chatty
  6 -> pure Debug
  7 -> pure Vomit
  other -> fail ("invalid verbosity level:" <> show other)

parseActivityType :: A.Value -> A.Parser ActivityType
parseActivityType = A.withScientific "ActivityType" \case
  0 -> pure UnknownType
  100 -> pure CopyPathType
  101 -> pure FileTransferType
  102 -> pure RealiseType
  103 -> pure CopyPathsType
  104 -> pure BuildsType
  105 -> pure BuildType
  106 -> pure OptimiseStoreType
  107 -> pure VerifyPathsType
  108 -> pure SubstituteType
  109 -> pure QueryPathInfoType
  110 -> pure PostBuildHookType
  111 -> pure BuildWaitingType
  112 -> pure FetchTreeType
  other -> fail ("invalid activity type: " <> show other)

parseAction :: A.Value -> A.Parser NixJSONMessage
parseAction val =
  val & A.withObject "Action" \obj -> do
    action :: Text <- obj A..: "action"
    ( \case
        "start" -> Start <$> parseStartAction val
        "stop" -> Stop <$> parseStopAction val
        "result" -> Result <$> parseResultAction val
        "msg" -> Message <$> parseMessageAction val
        other -> fail ("unknown action type: " <> show other)
      )
      action

parseMessageAction :: A.Value -> A.Parser MessageAction
parseMessageAction = A.withObject "MessageAction" \obj -> do
  level <- obj A..: "level" >>= parseVerbosity
  message <- obj A..: "msg"
  pure MkMessageAction{..}

one :: (MonadFail m) => m [b] -> m b
one listdec = do
  fields <- listdec
  case fields of
    [field] -> pure field
    _ -> fail "expected one field"

two :: (MonadFail m) => m [b] -> m (b, b)
two listdec = do
  fields <- listdec
  case fields of
    [field1, field2] -> pure (field1, field2)
    _ -> fail "expected two fields"

three :: (MonadFail m) => m [b] -> m (b, b, b)
three listdec = do
  fields <- listdec
  case fields of
    [field1, field2, field3] -> pure (field1, field2, field3)
    _ -> fail "expected three fields"

four :: (MonadFail m) => m [b] -> m (b, b, b, b)
four listdec = do
  fields <- listdec
  case fields of
    [field1, field2, field3, field4] -> pure (field1, field2, field3, field4)
    _ -> fail "expected four fields"

parseResultAction :: A.Value -> A.Parser ResultAction
parseResultAction = A.withObject "ResultAction" \obj -> do
  idField <- MkId <$> obj A..: "id"
  type' <- obj A..: "type"
  let
    fields :: (A.FromJSON a) => A.Parser a
    fields = obj A..: "fields"

  result <- case type' :: Int of
    100 -> uncurry FileLinked <$> fields
    101 -> BuildLogLine <$> fields
    102 -> UntrustedPath <$> (fields >>= parseStorePath)
    103 -> CorruptedPath <$> (fields >>= parseStorePath)
    104 -> SetPhase <$> fields
    105 -> (\(done, expected, running, failed) -> Progress (MkActivityProgress{..})) <$> fields
    106 -> do
      (typeNum, number) <- fields
      activityType <- parseActivityType typeNum
      pure $ SetExpected activityType number
    107 -> PostBuildLogLine <$> fields
    108 -> FetchStatus <$> fields
    other -> fail ("invalid activity result type: " <> show other)
  pure MkResultAction{id = idField, result}

parseStopAction :: A.Value -> A.Parser StopAction
parseStopAction = A.withObject "StopAction" \obj -> MkStopAction . MkId <$> obj A..: "id"

parseStartAction :: A.Value -> A.Parser StartAction
parseStartAction = A.withObject "StartAction11" \obj -> do
  idField <- obj A..: "id"
  text <- obj A..: "text"
  level <- obj A..: "level" >>= parseVerbosity
  activityType <- obj A..: "type" >>= parseActivityType

  let
    fields :: (A.FromJSON a) => A.Parser a
    fields = obj A..: "fields"

  activity <- case activityType of
    UnknownType -> pure Unknown
    CopyPathType ->
      fields >>= \(path, from, to) -> do
        path' <- parseStorePath path
        pure $ CopyPath path' (parseHost from) (parseHost to)
    FileTransferType -> FileTransfer <$> fields
    RealiseType -> pure Realise
    CopyPathsType -> pure CopyPaths
    BuildsType -> pure Builds
    BuildType ->
      fields >>= \(path, host, _ :: A.Value, _ :: A.Value) -> do
        path' <- parseDerivation path
        pure $ Build path' (parseHost host)
    OptimiseStoreType -> pure OptimiseStore
    VerifyPathsType -> pure VerifyPaths
    SubstituteType ->
      fields >>= \(path, host) -> do
        path' <- parseStorePath path
        pure $ Substitute path' (parseHost host)
    QueryPathInfoType ->
      fields >>= \(path, host) -> do
        path' <- parseStorePath path
        pure $ QueryPathInfo path' (parseHost host)
    PostBuildHookType -> PostBuildHook <$> (fields >>= parseDerivation)
    BuildWaitingType -> pure BuildWaiting
    FetchTreeType -> pure FetchTree
  pure MkStartAction{id = MkId idField, text, activity, level}
