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
    FileTransferType -> FileTransfer <$> one fields
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

-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/mgqqr8smdf746jc1zyblqffxpa4vsma0.narinfo\"],\"id\":1815667359615876,\"level\":4,\"parent\":1815667359615875,\"text\":\"downloading 'https://cache.nixos.org/mgqqr8smdf746jc1zyblqffxpa4vsma0.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/nfyy24xmlyqwby0qjz2kmd4py22m65gx.narinfo\"],\"id\":1815667359615878,\"level\":4,\"parent\":1815667359615877,\"text\":\"downloading 'https://cache.nixos.org/nfyy24xmlyqwby0qjz2kmd4py22m65gx.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/mpdm8z481z5p3fj9vy16lj92xxq6gy3v.narinfo\"],\"id\":1815667359615880,\"level\":4,\"parent\":1815667359615879,\"text\":\"downloading 'https://cache.nixos.org/mpdm8z481z5p3fj9vy16lj92xxq6gy3v.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/qh6x0q73dhb2fbvkbzf3ylawaa48hwfk.narinfo\"],\"id\":1815667359615882,\"level\":4,\"parent\":1815667359615881,\"text\":\"downloading 'https://cache.nixos.org/qh6x0q73dhb2fbvkbzf3ylawaa48hwfk.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/r473gx37fanq08d1j9kgy1a7g5qvlibn.narinfo\"],\"id\":1815667359615884,\"level\":4,\"parent\":1815667359615883,\"text\":\"downloading 'https://cache.nixos.org/r473gx37fanq08d1j9kgy1a7g5qvlibn.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/p8cgbhf15lcmzjjydzg4nljxwr3h6s98.narinfo\"],\"id\":1815667359615886,\"level\":4,\"parent\":1815667359615885,\"text\":\"downloading 'https://cache.nixos.org/p8cgbhf15lcmzjjydzg4nljxwr3h6s98.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/rbnjj6ayzb4iwsjnms8644k8v3lbfvz0.narinfo\"],\"id\":1815667359615888,\"level\":4,\"parent\":1815667359615887,\"text\":\"downloading 'https://cache.nixos.org/rbnjj6ayzb4iwsjnms8644k8v3lbfvz0.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/x6l9nl5f4gjhqp435n5zca8i9clcfiiy.narinfo\"],\"id\":1815667359615890,\"level\":4,\"parent\":1815667359615889,\"text\":\"downloading 'https://cache.nixos.org/x6l9nl5f4gjhqp435n5zca8i9clcfiiy.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/zj917zf1rryj0w1x2lpgh9jswiczanq6.narinfo\"],\"id\":1815667359615892,\"level\":4,\"parent\":1815667359615891,\"text\":\"downloading 'https://cache.nixos.org/zj917zf1rryj0w1x2lpgh9jswiczanq6.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/zhipniwspvbrn0dbr7xnh618mb5q0q1r.narinfo\"],\"id\":1815667359615894,\"level\":4,\"parent\":1815667359615893,\"text\":\"downloading 'https://cache.nixos.org/zhipniwspvbrn0dbr7xnh618mb5q0q1r.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/zb0mqwvxzlqsax893siwkv3l0nh953n9.narinfo\"],\"id\":1815667359615896,\"level\":4,\"parent\":1815667359615895,\"text\":\"downloading 'https://cache.nixos.org/zb0mqwvxzlqsax893siwkv3l0nh953n9.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/zx6vhfdwi2imxrciqv168hsxsl4qc5cm.narinfo\"],\"id\":1815667359615898,\"level\":4,\"parent\":1815667359615897,\"text\":\"downloading 'https://cache.nixos.org/zx6vhfdwi2imxrciqv168hsxsl4qc5cm.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/vmszsjf14lrbcdmhbzl59zn44am26fqr.narinfo\"],\"id\":1815667359615900,\"level\":4,\"parent\":1815667359615899,\"text\":\"downloading 'https://cache.nixos.org/vmszsjf14lrbcdmhbzl59zn44am26fqr.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/y8wpbzrc9r6kl6rm81pf8qws38bzic4j.narinfo\"],\"id\":1815667359615902,\"level\":4,\"parent\":1815667359615901,\"text\":\"downloading 'https://cache.nixos.org/y8wpbzrc9r6kl6rm81pf8qws38bzic4j.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/7xa08b1m1nll7cbf29ji7lr1ngw9ygc4.narinfo\"],\"id\":1815667359615904,\"level\":4,\"parent\":1815667359615903,\"text\":\"downloading 'https://cache.nixos.org/7xa08b1m1nll7cbf29ji7lr1ngw9ygc4.narinfo'\",\"type\":101}"
-- [Error] Could not deserialize: Right "@nix {\"action\":\"start\",\"fields\":[\"https://cache.nixos.org/88h43v1r0wilxyr5v7ji86f582128z25.narinfo\"],\"id\":1815667359615906,\"level\":4,\"parent\":1815667359615905,\"text\":\"downloading 'https://cache.nixos.org/88h43v1r0wilxyr5v7ji86f582128z25.narinfo'\",\"type\":101}"
