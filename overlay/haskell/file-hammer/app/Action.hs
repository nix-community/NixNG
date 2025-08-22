{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ViewPatterns #-}

module Action where

import Config (Content (..))
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Numeric.Extra (showIntAtBase)
import Path.Posix (Abs, Dir, File, Path, Rel, toFilePath)
import System.IO.Extra (hFlush)
import System.Posix (COff (..), OpenMode (ReadOnly), createDirectory, fdToHandle, removeDirectory)
import System.Posix.Files (fileSize, getFdStatus, removeLink, setFdMode, setFdOwnerAndGroup)
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (CMode, Fd (..))
import System.Posix.User (getGroupEntryForName, getUserEntryForName, groupID, userID)

data SomePath b = forall t. SomePath (Path b t)

instance Show (SomePath Abs) where
  show (SomePath path) = show path

instance Show (SomePath Rel) where
  show (SomePath path) = show path

data Action
  = Action'Chown {user :: Text, group :: Text, path :: SomePath Abs}
  | Action'Chmod {mode :: CMode, path :: SomePath Abs}
  | Action'DeleteFile {filePath :: Path Abs File}
  | Action'CreateFile {filePath :: Path Abs File, content :: Content, user :: Text, group :: Text, mode :: CMode}
  | Action'UpdateFile {filePath :: Path Abs File, content :: Content}
  | Action'DeleteDirectory {dirPath :: Path Abs Dir}
  | Action'CreateDirectory {dirPath :: Path Abs Dir, user :: Text, group :: Text, mode :: CMode}
  deriving (Show)

newtype Action'Show = Action'Show Action

instance Show Action'Show where
  show (Action'Show (Action'UpdateFile{filePath, content = _})) = "Action'UpdateFile { filePath = " <> show filePath <> ", content = <<omitted>> }"
  show (Action'Show (Action'CreateFile{filePath, content = _, user, group, mode})) =
    "Action'CreateFile { filePath = "
      <> show filePath
      <> ", content = <<omitted>>, user = "
      <> show user
      <> ", group = "
      <> show group
      <> ", mode = "
      <> show mode
      <> " }"
  show (Action'Show action) = show action

-- | Convert a `Path` to `Text`.
toFilePathText :: Path b t -> Text
toFilePathText = T.pack . toFilePath

data Command = Command {cmdline :: [Text], stdin :: Maybe Content}

actionToCommand :: Action -> Command
actionToCommand Action'Chown{user, group, path} =
  Command
    { cmdline =
        [ "chown"
        , user
        , ":"
        , group
        , " "
        , case path of SomePath path' -> toFilePathText path'
        ]
    , stdin = Nothing
    }
actionToCommand Action'Chmod{path, mode} =
  Command
    { cmdline =
        [ "chmod"
        , T.pack (showIntAtBase 8 intToDigit mode "")
        , case path of
            SomePath path' -> toFilePathText path'
        ]
    , stdin = Nothing
    }
actionToCommand Action'DeleteFile{filePath} =
  Command
    { cmdline = ["rm", toFilePathText filePath]
    , stdin = Nothing
    }
actionToCommand Action'CreateFile{filePath, content, user, group, mode} =
  Command
    { cmdline =
        [ "install"
        , "--no-target-directory"
        , "--owner=" <> user
        , "--group=" <> group
        , "--mode=" <> T.pack (showIntAtBase 8 intToDigit mode "")
        ]
          <> case content of
            ContentText _ -> ["/dev/stdin"]
            ContentBinary _ -> ["/dev/stdin"]
            ContentFile path -> [toFilePathText path]
            _ -> []
          <> [ toFilePathText filePath
             ]
    , stdin = Just content
    }
actionToCommand Action'UpdateFile{filePath, content} =
  Command
    { cmdline = ["install", "/dev/stdin", toFilePathText filePath]
    , stdin = Just content
    }
actionToCommand Action'DeleteDirectory{dirPath} =
  Command
    { cmdline = ["rm", "--recursive", toFilePathText dirPath]
    , stdin = Nothing
    }
actionToCommand Action'CreateDirectory{dirPath} =
  Command
    { cmdline = ["mkdir", toFilePathText dirPath]
    , stdin = Nothing
    }

commandToText :: Command -> Text
commandToText Command{cmdline, stdin} =
  "> " <> T.intercalate " " cmdline <> case stdin of
    Just ContentAny -> ""
    Just (ContentText text) -> " <<\"EOF\"\n" <> text <> "EOF"
    Just (ContentBinary _) -> T.unlines [" <<\"EOF\"", "<<binary>>", "EOF"]
    Just (ContentFile _) -> ""
    Nothing -> ""

foreign import capi "unistd.h copy_file_range"
  copy_file_range :: Fd -> Ptr COff -> Fd -> Ptr COff -> COff -> CUInt -> IO ()

writeContent :: Fd -> Content -> IO ()
writeContent _fd ContentAny = pure ()
writeContent fd (ContentBinary bs) = fdToHandle fd >>= \handle -> BS.hPut handle bs >> hFlush handle
writeContent fd (ContentText text) = fdToHandle fd >>= \handle -> T.hPutStr handle text >> hFlush handle
writeContent fd (ContentFile file) = bracket (openFd (toFilePath file) ReadOnly defaultFileFlags) closeFd \sourceFd -> do
  status <- getFdStatus sourceFd
  copy_file_range sourceFd nullPtr fd nullPtr (fileSize status) 0

runAction :: Action -> IO ()
runAction Action'Chown{user, group, path = SomePath (toFilePath -> path)} = bracket (openFd path WriteOnly defaultFileFlags) closeFd \fd -> do
  userEntry <- getUserEntryForName (T.unpack user)
  groupEntry <- getGroupEntryForName (T.unpack group)

  setFdOwnerAndGroup fd (userID userEntry) (groupID groupEntry)
runAction Action'Chmod{mode, path = SomePath (toFilePath -> path)} = bracket (openFd path WriteOnly defaultFileFlags) closeFd \fd -> do
  setFdMode fd mode
runAction Action'DeleteFile{filePath} = removeLink (toFilePath filePath)
runAction Action'CreateFile{filePath, content, user, group, mode} = bracket (openFd (toFilePath filePath) WriteOnly fileFlags) closeFd \fd -> do
  userEntry <- getUserEntryForName (T.unpack user)
  groupEntry <- getGroupEntryForName (T.unpack group)

  writeContent fd content

  setFdOwnerAndGroup fd (userID userEntry) (groupID groupEntry)
 where
  fileFlags = defaultFileFlags{creat = Just mode, trunc = True}
runAction Action'UpdateFile{filePath, content} = bracket (openFd (toFilePath filePath) WriteOnly fileFlags) closeFd \fd -> do
  putStrLn "update file"
  print content
  writeContent fd content
 where
  fileFlags = defaultFileFlags{trunc = True}
runAction Action'DeleteDirectory{dirPath} = removeDirectory (toFilePath dirPath)
runAction Action'CreateDirectory{dirPath, user, group, mode} = bracket (openFd (toFilePath dirPath) WriteOnly fileFlags) closeFd \fd -> do
  userEntry <- getUserEntryForName (T.unpack user)
  groupEntry <- getGroupEntryForName (T.unpack group)

  setFdOwnerAndGroup fd (userID userEntry) (groupID groupEntry)
 where
  fileFlags = defaultFileFlags{creat = Just mode, directory = True}
