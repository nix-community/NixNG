{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Nix.Select where

import Control.Applicative ((<|>))
import Control.Exception (Deadlock (Deadlock), Exception)
import Control.Monad (forM, void)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as A
import Data.Aeson.Decoding qualified as A
import Data.Aeson.Text qualified as A
import Data.Aeson.Types (FromJSON)
import Data.Aeson.Types qualified as A
import Data.Attoparsec.Text.Lazy
import Data.ByteString.Lazy (ByteString, LazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TL
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLensesWith)
import Nix.Derivation (Derivation, parseDerivation, parseDerivationWith, textParser)
import Path.Posix (Abs, Dir, File, Path, parseAbsDir, parseAbsFile, toFilePath)
import System.Exit (ExitCode (..))
import System.IO (Handle, stdin)
import System.NixNG.SomePath (SomePath (..))
import System.NixNG.TH (duplicateRules)
import System.Posix.Files (getFileStatus, isRegularFile, isSymbolicLink)
import System.Process.Typed (
  byteStringOutput,
  createPipe,
  getStderr,
  getStdout,
  proc,
  readProcess,
  readProcessStdout,
  setStderr,
  setStdout,
  startProcess,
  stopProcess,
  waitExitCodeSTM,
  withProcess,
  withProcessTerm,
  withProcessWait,
 )
import UnliftIO (MonadUnliftIO, bracket, throwIO)
import UnliftIO.STM (atomically)

data NixError
  = NixErrorParse Text
  | NixErrorExec ExitCode ByteString
  | NixErrorOther Text
  deriving stock (Show)

instance Exception NixError

data FlakeInfoLocked = FlakeInfoLocked
  { narHash :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (A.FromJSON)

data FlakeInfo = FlakeInfo
  { fingerprint :: Maybe Text
  , lastModified :: Int
  , locked :: FlakeInfoLocked
  , path :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (A.FromJSON)

data Selector = SelectorAll | SelectorSet [Selector] | SelectorStr Text | SelectorMaybe Text
  deriving stock (Generic)

selectorOptions :: A.Options
selectorOptions =
  A.defaultOptions
    { A.sumEncoding =
        A.TaggedObject
          { A.tagFieldName = "type"
          , contentsFieldName = "value"
          }
    , A.constructorTagModifier = map toLower . fromJust . stripPrefix "Selector"
    }

instance A.ToJSON Selector where
  toJSON v = A.genericToJSON selectorOptions v
instance A.FromJSON Selector where
  parseJSON v = A.genericParseJSON selectorOptions v

data NixTargetCommand = Build | Eval

data NixTarget a where
  NixExpr :: Text -> NixTarget a
  NixFlake :: Text -> NixTarget a
  NixDerivation :: Path Abs File -> NixTarget Build

makeLensesWith duplicateRules ''FlakeInfo
makeLensesWith duplicateRules ''FlakeInfoLocked

nixProc
  :: (MonadIO m, MonadThrow m, MonadUnliftIO m)
  => [Text] -> (Handle -> m ()) -> m LazyByteString
nixProc args logFn = do
  (exitCode, stdout) <- withProcessTerm config \p -> do
    logFn . getStderr $ p
    atomically $
      (,)
        <$> waitExitCodeSTM p
        <*> getStdout p
  case exitCode of
    ExitSuccess -> pure stdout
    _ -> throwM $ NixErrorExec exitCode stdout
 where
  config =
    setStdout byteStringOutput
      . setStderr createPipe
      $ proc "nix" (["--log-format", "internal-json"] <> map T.unpack args)

nixFlakeInfo :: (Monad m, MonadThrow m, MonadThrow m, MonadUnliftIO m) => Text -> (Handle -> m ()) -> m FlakeInfo
nixFlakeInfo flakePath logFn =
  nixProc ["flake", "metadata", "--json", flakePath] logFn >>= \bs -> case A.eitherDecode bs of
    Right flakeInfo -> pure flakeInfo
    Left err -> throwM . NixErrorParse $ T.pack err

nixCopy
  :: (MonadIO m, MonadThrow m, MonadUnliftIO m) => SomePath Abs -> Text -> Text -> (Handle -> m ()) -> m ()
nixCopy storePath remoteUser targetHost logFn = void $ nixProc args logFn
 where
  storePath' = case storePath of SomePath path -> T.pack $ toFilePath path
  args = ["copy", storePath', "--to", "ssh://" <> remoteUser <> "@" <> targetHost]

nixBuild
  :: (MonadIO m, MonadThrow m, MonadUnliftIO m)
  => NixTarget Build -> (Handle -> m ()) -> m [Either (Path Abs Dir) (Path Abs File)]
nixBuild nixTarget logFn =
  nixProc args logFn >>= \stdout ->
    forM (TL.lines $ decodeUtf8 stdout) \path -> do
      status <- liftIO $ getFileStatus (TL.unpack path)

      if isRegularFile status || isSymbolicLink status
        then
          parseAbsFile (TL.unpack path) <&> Right
        else
          parseAbsDir (TL.unpack path) <&> Left
 where
  args =
    ["build", "--show-trace", "--no-link", "--print-out-paths"] <> case nixTarget of
      NixExpr expression -> ["--expr", expression]
      NixFlake flake -> [flake]
      NixDerivation derivation -> [T.pack $ toFilePath derivation <> "^*"]

nixEval
  :: forall a m
   . (FromJSON a, MonadThrow m, MonadThrow m, MonadUnliftIO m)
  => NixTarget Eval -> (Handle -> m ()) -> m a
nixEval nixTarget logFn =
  nixProc args logFn <&> fromJust . A.decode
 where
  args =
    ["eval", "--show-trace", "--json"] <> case nixTarget of
      NixExpr expression -> ["--expr", expression]
      NixFlake flake -> [flake]

nixDerivationShow
  :: (MonadThrow m, MonadUnliftIO m)
  => Path Abs File -> m (Derivation Text Text)
nixDerivationShow derivation =
  liftIO (TL.readFile (toFilePath derivation))
    >>= \drv -> either (throwIO . NixErrorParse . T.pack) pure (parseOnly parseDerivation' drv)
 where
  parseDerivation' = parseDerivationWith textParser textParser

select
  :: (A.FromJSON b, MonadIO m, MonadThrow m, MonadUnliftIO m)
  => [([Selector], A.Value -> A.Value)] -> Text -> (Handle -> m ()) -> m [b]
select selectors flakeRef logFn = do
  selectLib <- nixFlakeInfo "https://git.clan.lol/clan/nix-select/archive/main.tar.gz" logFn
  flake <- nixFlakeInfo flakeRef logFn

  let expression =
        T.unlines
          [ "let"
          , "  flake = builtins.getFlake \"path:" <> flake ^. _path <> "?narHash=" <> flake ^. _locked . _narHash <> "\";"
          , "  selectLib = ("
          , "    builtins.getFlake"
          , "      \"path:" <> selectLib ^. _path <> "?narHash=" <> selectLib ^. _locked . _narHash <> "\""
          , "  ).lib;"
          , "in"
          , "  derivation {"
          , "    name = \"clan-flake-select\";"
          , "    result = builtins.toJSON ["
          , selectors'
          , "    ];"
          , ""
          , "    # We can always build this derivation locally, since /bin/sh is system independent,"
          , "    # remote builders would introduce needless overhead."
          , "    preferLocalBuild = true;"
          , "    # Save the roundtrip to check the binary caches for trival substitutions"
          , "    allowSubstitutes = false;"
          , ""
          , "    passAsFile = [ \"result\" ];"
          , "    system = \"" <> system <> "\";"
          , "    builder = \"/bin/sh\";"
          , "    args = ["
          , "      \"-c\""
          , "      ''"
          , "         read -r x < \"$resultPath\"; printf %s \"$x\" > $out"
          , "      ''"
          , "    ];"
          , "  }"
          ]

  paths <- nixBuild (NixExpr expression) logFn

  results :: [A.Value] <- case paths of
    [Right file] ->
      liftIO $
        A.eitherDecodeFileStrict (toFilePath file) >>= \case
          Left err -> throwM $ A.AesonException err
          Right res -> pure res
    _ -> throwM $ NixErrorOther "expected single file output"

  mapM
    ( \(v, fn) -> case A.fromJSON (fn v) of
        A.Success a -> pure a
        A.Error err -> throwM $ A.AesonException err
    )
    $ zip results (map snd selectors)
 where
  system = "x86_64-linux"
  selectors' =
    T.intercalate " "
      . map
        (\attr -> "(selectLib.applySelectors (builtins.fromJSON ''" <> TL.toStrict (A.encodeToLazyText attr) <> "'') flake)")
      $ map fst selectors
