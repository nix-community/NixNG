{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Action where

import Config (Content (..))
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Monoid.Extra (mwhen)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Foreign.C (CChar)
import Foreign.C.Error (Errno (..), eXDEV, getErrno)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Numeric.Extra (showIntAtBase)
import Path.Posix (Abs, Dir, File, Path, Rel, toFilePath)
import SomePath (SomePath (..))
import System.Directory.Extra (removeDirectoryRecursive)
import System.IO.Extra (hFlush)
import System.Posix (COff (..), OpenMode (ReadOnly), createDirectory, fdToHandle, removeDirectory)
import System.Posix.Files (
  createSymbolicLink,
  fileSize,
  getFdStatus,
  getFileStatus,
  isDirectory,
  removeLink,
  rename,
  setFdMode,
  setFdOwnerAndGroup,
  setFileMode,
  setOwnerAndGroup,
 )
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (CMode, Fd (..))
import System.Posix.User (getGroupEntryForName, getUserEntryForName, groupID, userID)

data Action
  = Action'Chown {user :: Text, group :: Text, path :: SomePath Abs}
  | Action'Chmod {mode :: CMode, path :: SomePath Abs}
  | Action'DeleteFile {filePath :: Path Abs File}
  | Action'CreateFile {filePath :: Path Abs File, content :: Content, user :: Text, group :: Text, mode :: CMode}
  | Action'UpdateFile {filePath :: Path Abs File, content :: Content}
  | Action'DeleteDirectory {dirPath :: Path Abs Dir}
  | Action'CreateDirectory {dirPath :: Path Abs Dir, user :: Text, group :: Text, mode :: CMode}
  | Action'CreateLink {source :: Path Abs File, destination :: Text}
  | Action'UpdateLink {source :: Path Abs File, destination :: Text}
  | Action'DeleteLink {source :: Path Abs File}
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
        , user <> ":" <> group
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
actionToCommand Action'CreateLink{source, destination} =
  Command
    { cmdline = ["ln", "--no-target-directory", "--symbolic", destination, toFilePathText source]
    , stdin = Nothing
    }
actionToCommand Action'UpdateLink{source, destination} =
  Command
    { cmdline = ["ln", "--no-target-directory", "--symbolic", "--force", destination, toFilePathText source]
    , stdin = Nothing
    }
actionToCommand Action'DeleteLink{source} =
  Command
    { cmdline = ["rm", toFilePathText source]
    , stdin = Nothing
    }

commandToText :: Bool -> Command -> Text
commandToText verbose Command{cmdline, stdin} =
  "> "
    <> T.intercalate " " cmdline
    <> ( mwhen verbose $ case stdin of
           Just ContentAny -> ""
           Just (ContentText text) -> " <<\"EOF\"\n" <> text <> "EOF"
           Just (ContentBinary _) -> T.unlines [" <<\"EOF\"", "<<binary>>", "EOF"]
           Just (ContentFile _) -> ""
           Nothing -> ""
       )

foreign import capi "unistd.h copy_file_range"
  copy_file_range :: Fd -> Ptr COff -> Fd -> Ptr COff -> COff -> CUInt -> IO CInt
foreign import capi "sys/sendfile.h sendfile"
  sendfile :: Fd -> Fd -> Ptr COff -> COff -> IO CInt
foreign import capi "errno.h perror"
  perror :: Ptr CChar -> IO ()

writeContent :: Fd -> Content -> IO ()
writeContent _fd ContentAny = pure ()
writeContent fd (ContentBinary bs) = fdToHandle fd >>= \handle -> BS.hPut handle bs >> hFlush handle
writeContent fd (ContentText text) = fdToHandle fd >>= \handle -> T.hPutStr handle text >> hFlush handle
writeContent fd (ContentFile file) = bracket (openFd (toFilePath file) ReadOnly defaultFileFlags) closeFd \sourceFd -> do
  status <- getFdStatus sourceFd
  err <- copy_file_range sourceFd nullPtr fd nullPtr (fileSize status) 0
  when (err < 0) $
    getErrno >>= \case
      x | x == eXDEV -> do
        err' <- sendfile fd sourceFd nullPtr (fileSize status)
        when (err' < 0) $ withCString "send file" \msg -> perror msg
      _ -> withCString "copy_file_range failed with" \msg -> perror msg

logAction :: Action -> IO ()
logAction = T.putStrLn . commandToText False . actionToCommand

runAction :: Action -> IO ()
runAction action@Action'Chown{user, group, path = SomePath (toFilePath -> path)} =
  logAction action >> do
    userEntry <- getUserEntryForName (T.unpack user)
    groupEntry <- getGroupEntryForName (T.unpack group)

    setOwnerAndGroup path (userID userEntry) (groupID groupEntry)
runAction action@Action'Chmod{mode, path = SomePath (toFilePath -> path)} =
  logAction action >> setFileMode path mode
runAction action@Action'DeleteFile{filePath} =
  logAction action >> removeLink (toFilePath filePath)
runAction action@Action'CreateFile{filePath, content, user, group, mode} =
  logAction action >> bracket (openFd (toFilePath filePath) WriteOnly fileFlags) closeFd \fd -> do
    userEntry <- getUserEntryForName (T.unpack user)
    groupEntry <- getGroupEntryForName (T.unpack group)

    writeContent fd content

    setFdOwnerAndGroup fd (userID userEntry) (groupID groupEntry)
 where
  fileFlags = defaultFileFlags{creat = Just mode, trunc = True}
runAction action@Action'UpdateFile{filePath, content} =
  logAction action >> bracket (openFd (toFilePath filePath) WriteOnly fileFlags) closeFd \fd -> writeContent fd content
 where
  fileFlags = defaultFileFlags{trunc = True}
runAction action@Action'DeleteDirectory{dirPath} =
  logAction action >> removeDirectoryRecursive (toFilePath dirPath)
runAction action@Action'CreateDirectory{dirPath = toFilePath -> path, user, group, mode} =
  logAction action >> do
    createDirectory path mode

    userEntry <- getUserEntryForName (T.unpack user)
    groupEntry <- getGroupEntryForName (T.unpack group)

    setOwnerAndGroup path (userID userEntry) (groupID groupEntry)
runAction action@Action'CreateLink{source = toFilePath -> source, destination = T.unpack -> destination} =
  logAction action >> createSymbolicLink destination source
runAction action@Action'UpdateLink{source = toFilePath -> source, destination = T.unpack -> destination} =
  logAction action >> do
    createSymbolicLink destination (source <> ".new")
    rename (source <> ".new") source
runAction action@Action'DeleteLink{source = toFilePath -> source} =
  logAction action >> removeLink source
