{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Action (
  Action (
    Action'Chmod,
    Action'Chown,
    Action'CreateDirectory,
    Action'CreateFile,
    Action'CreateLink,
    Action'DeleteDirectory,
    Action'DeleteFile,
    Action'DeleteLink,
    Action'UpdateFile,
    Action'UpdateLink,
    content,
    destination,
    dirPath,
    filePath,
    group,
    mode,
    path,
    source,
    user
  ),
  actionToCommand,
  commandToText,
  runAction,
 )
import ByteString.Aeson.Orphans ()
import Cli (Cli (Cli), parseCli, _logLevel)
import Cli qualified
import Config (
  Content (ContentAny, ContentBinary, ContentFile, ContentText),
  DirectoryContent (DirectoryContentManaged, DirectoryContentUnmanaged, directories, files, links),
  DirectoryNode (DirectoryNode, content, mode, owner),
  FileNode (FileNode, content, mode, owner),
  Group (GroupId, GroupName),
  HasIgnores (),
  LinkNode (LinkNode, destination),
  Owner (Owner, group, user),
  Specification (Specification, directory, ignores),
  User (UserId, UserName),
  getUnmanaged,
  _content,
  _destination,
  _directories,
  _directory,
  _files,
  _group,
  _ignores,
  _links,
  _mode,
  _owner,
  _user,
 )
import Control.Exception (Exception)
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadCatch, MonadThrow, handleIf, throwM)
import Control.Monad.Extra ((&&^))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (
  LoggingT,
  MonadLogger,
  MonadLoggerIO,
  filterLogger,
  logDebugN,
  logWarnN,
  runStderrLoggingT,
 )
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (StateT, execStateT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.Either (lefts, rights)
import Data.Either.Extra (maybeToEither)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (find, inits)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.TreeDiff (ediff, prettyEditExprCompact)
import Lens.Micro (
  Field1 (_1),
  Field2 (_2),
  Field3 (_3),
  (<&>),
  (^.),
 )
import Lens.Micro.Mtl (view, (%=))
import Lens.Micro.TH (makeLensesWith)
import Path.Posix (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  absdir,
  dirname,
  parseRelDir,
  parseRelFile,
  reldir,
  toFilePath,
  (</>),
 )
import Path.Posix qualified as Posix
import SomePath (SomePath (SomePath))
import System.Directory (listDirectory)
import System.FilePath.Glob (Pattern, match)
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import System.Posix qualified as Posix
import System.Posix.Files (
  fileExist,
  fileGroup,
  fileMode,
  fileOwner,
  getFileStatus,
  getSymbolicLinkStatus,
  isDirectory,
  isRegularFile,
  isSymbolicLink,
  readSymbolicLink,
 )
import System.Posix.Types (FileMode)
import System.Posix.User (
  getGroupEntryForID,
  getUserEntryForID,
  groupName,
  userName,
 )
import TH (duplicateRules)
import Text.PrettyPrint (render)

data FilterInfo = FilterInfo
  { unmanaged :: HashSet (SomePath Rel)
  , ignores :: HashSet Pattern
  , root :: Path Abs Dir
  }

makeLensesWith duplicateRules ''FilterInfo

type GetHashMaps =
  ( HashMap (Path Rel File) FileNode
  , HashMap (Path Rel File) LinkNode
  , HashMap (Path Rel Dir) DirectoryNode
  )

getHashMaps
  :: ( MonadCatch m
     , MonadIO m
     , MonadLogger m
     , MonadReader FilterInfo m
     , MonadThrow m
     )
  => Path Rel Dir
  -> [FilePath]
  -> StateT GetHashMaps m ()
getHashMaps path paths = do
  ignores <- view _ignores
  root <- view _root
  let
    filteredEntries = map (\name -> maybeToEither name $ find (`match` (toFilePath path <> name)) ignores <&> (name,)) paths

  logDebugN ("ignored entries: " <> (T.show . map (^. _1) $ rights filteredEntries))

  forM_ (lefts filteredEntries) \ent -> do
    let entPath = toFilePath (root </> path) <> ent
    status <- io $ getSymbolicLinkStatus entPath

    if
      | isRegularFile status -> do
          name <- parseRelFile ent
          fileNode <- readFileNode $ path </> name

          _1
            %= HM.insert
              name
              fileNode
      | isSymbolicLink status -> do
          name <- parseRelFile ent
          destination <- io . readSymbolicLink . toFilePath $ root </> path </> name
          _2 %= HM.insert name LinkNode{destination = T.pack destination}
      | isDirectory status -> do
          name <- parseRelDir ent
          dirNode <- readDir $ path </> name

          _3
            %= HM.insert
              name
              dirNode
      | otherwise -> logWarnN $ T.show ent <> " is of an unknown type"

readContent
  :: ( MonadIO m
     , MonadLogger m
     )
  => Path Abs File -> m Content
readContent path =
  io (BS.readFile (toFilePath path)) >>= \bs -> case T.decodeUtf8' bs of
    Left _unicodeException -> do
      logDebugN ("content of " <> T.show path <> " looks binary")
      pure $ ContentBinary bs
    Right text -> pure $ ContentText text

getUserFromStatus :: (MonadCatch m, MonadIO m) => Posix.FileStatus -> m User
getUserFromStatus (fileOwner -> userId) = handleIf predicate fallback action
 where
  predicate = isDoesNotExistErrorType . ioeGetErrorType
  fallback = const . pure . UserId $ userId
  action = io $ getUserEntryForID userId <&> UserName . T.pack . userName

getGroupFromStatus :: (MonadCatch m, MonadIO m) => Posix.FileStatus -> m Group
getGroupFromStatus (fileGroup -> groupId) = handleIf predicate fallback action
 where
  predicate = isDoesNotExistErrorType . ioeGetErrorType
  fallback = const . pure . GroupId $ groupId
  action = io $ getGroupEntryForID groupId <&> GroupName . T.pack . groupName

permissionMask :: Posix.CMode
permissionMask = 0b111111111

readFileNode
  :: ( MonadCatch m
     , MonadIO m
     , MonadLogger m
     , MonadReader FilterInfo m
     , MonadThrow m
     )
  => Path Rel File -> m FileNode
readFileNode path = do
  root <- view _root
  unmanaged <- view _unmanaged

  status <- io $ getFileStatus (toFilePath $ root </> path)

  user <- getUserFromStatus status
  group <- getGroupFromStatus status

  fileContent <-
    if SomePath path `HS.member` unmanaged
      then do
        logDebugN ("scraping file " <> T.show path <> " without content")
        pure ContentAny
      else do
        logDebugN ("scraping file " <> T.show path <> " with content")
        readContent (root </> path)

  pure $
    FileNode
      { owner =
          Owner
            { user
            , group
            }
      , mode = fileMode status .&. permissionMask
      , content = fileContent
      }

readDir
  :: ( MonadCatch m
     , MonadIO m
     , MonadLogger m
     , MonadReader FilterInfo m
     , MonadThrow m
     )
  => Path Rel Dir -> m DirectoryNode
readDir path = do
  unmanaged <- view _unmanaged
  root <- view _root
  content <-
    if SomePath path `HS.member` unmanaged
      then do
        logDebugN ("not recursing into directory " <> T.show path <> " due to being unmanaged")
        pure DirectoryContentUnmanaged
      else do
        logDebugN ("recursing into directory " <> T.show path)
        (file', link', directory') <-
          (io . listDirectory . toFilePath $ root </> path)
            >>= (`execStateT` (HM.empty, HM.empty, HM.empty)) . getHashMaps path

        pure $
          DirectoryContentManaged
            { files = file'
            , links = link'
            , directories = directory'
            }

  status <- io . getFileStatus . toFilePath $ root </> path

  user <- getUserFromStatus status
  group <- getGroupFromStatus status

  pure
    DirectoryNode
      { owner =
          Owner
            { user = user
            , group = group
            }
      , mode = fileMode status .&. permissionMask
      , content
      }

modifyFile
  :: (MonadLoggerIO m, MonadWriter [Action] m)
  => Path Abs Dir -> Path Rel File -> (FileNode, FileNode) -> m ()
modifyFile path name (desired, actual) = do
  when (desired ^. _owner /= actual ^. _owner) $
    tell
      [Action'Chown{path = SomePath $ path </> name, user = desired ^. _owner . _user, group = desired ^. _owner . _group}]
  when (desired ^. _mode /= actual ^. _mode) $
    tell [Action'Chmod{path = SomePath $ path </> name, mode = desired ^. _mode}]

  desiredContent <- case (desired ^. _content) of
    (ContentFile contentPath) -> readContent contentPath
    _ -> pure $ actual ^. _content
  when (desiredContent /= actual ^. _content) $
    tell [Action'UpdateFile{filePath = path </> name, content = desired ^. _content}]

createFile
  :: (MonadWriter [Action] m)
  => Path Abs Dir -> Path Rel File -> FileNode -> m ()
createFile path name desired =
  tell
    [ Action'CreateFile
        { filePath = path </> name
        , content = desired ^. _content
        , user = desired ^. _owner . _user
        , group = desired ^. _owner . _group
        , mode = desired ^. _mode
        }
    ]

deleteFile
  :: ( MonadReader FilterInfo m
     , MonadWriter [Action] m
     )
  => Path Abs Dir -> Path Rel File -> FileNode -> m ()
deleteFile root name _ =
  tell [Action'DeleteFile{filePath = root </> name}]

excludeByName :: (Foldable f) => f Pattern -> Path Rel t -> Bool
excludeByName exclusions (toFilePath -> name) = not $ any (`match` name') exclusions
 where
  name' = if last name == '/' then init name else name

createDirectory
  :: ( MonadReader FilterInfo m
     , MonadWriter [Action] m
     )
  => Path Abs Dir -> Path Rel Dir -> DirectoryNode -> m ()
createDirectory path name desired = do
  ignores <- view _ignores
  let
    path' = path </> name
    filterByExclusions = filter \(item, _) -> excludeByName ignores item

  tell
    [ Action'CreateDirectory
        { dirPath = path'
        , user = desired ^. _owner . _user
        , group = desired ^. _owner . _group
        , mode = desired ^. _mode
        }
    ]

  case desired ^. _content of
    managed@DirectoryContentManaged{} -> do
      forM_ (filterByExclusions . HM.toList $ managed ^. _files) . uncurry $ createFile path'
      forM_ (filterByExclusions . HM.toList $ managed ^. _directories) . uncurry $ createDirectory path'
      forM_ (filterByExclusions . HM.toList $ managed ^. _links) . uncurry $ createLink path'
    DirectoryContentUnmanaged -> pure ()

deleteDirectory
  :: (MonadWriter [Action] m)
  => Path Abs Dir -> Path Rel Dir -> DirectoryNode -> m ()
deleteDirectory path name _actual =
  tell [Action'DeleteDirectory{dirPath = path </> name}]

createLink
  :: (MonadWriter [Action] m)
  => Path Abs Dir -> Path Rel File -> LinkNode -> m ()
createLink path name desired = tell [Action'CreateLink{source = path </> name, destination = desired ^. _destination}]

modifyLink
  :: (MonadWriter [Action] m)
  => Path Abs Dir -> Path Rel File -> (LinkNode, LinkNode) -> m ()
modifyLink path name (desired, _actual) = tell [Action'UpdateLink{source = path </> name, destination = desired ^. _destination}]

deleteLink
  :: (MonadWriter [Action] m)
  => Path Abs Dir -> Path Rel File -> LinkNode -> m ()
deleteLink path name _actual = tell [Action'DeleteLink{source = path </> name}]

hashmapCompare
  :: ( Eq v
     , MonadIO m
     , MonadLoggerIO m
     , Show v
     )
  => HashSet Pattern
  -> HashMap (Path Rel t) v
  -> HashMap (Path Rel t) v
  -> (Path Rel t -> v -> m ())
  -> (Path Rel t -> (v, v) -> m ())
  -> (Path Rel t -> v -> m ())
  -> m ()
hashmapCompare ignores first second create modify delete = do
  let
    first' = (`HM.filterWithKey` first) \name _ -> excludeByName ignores name
    second' = (`HM.filterWithKey` second) \name _ -> excludeByName ignores name

    excludedNames = (first `HM.difference` first') `HM.union` (second `HM.difference` second')

    deleted = first' `HM.difference` second'
    created = second' `HM.difference` first'
    updated = HM.filter (uncurry (/=)) $ HM.intersectionWith (,) second' first'

  logDebugN ("excluded names: " <> T.show excludedNames)

  forM_ (HM.toList created) . uncurry $ create
  forM_ (HM.toList updated) . uncurry $ modify
  forM_ (HM.toList deleted) . uncurry $ delete

modifyDirectory
  :: ( MonadLoggerIO m
     , MonadReader FilterInfo m
     )
  => Path Abs Dir
  -> (DirectoryNode, DirectoryNode)
  -> WriterT [Action] m ()
modifyDirectory path (desired, actual) = do
  ignores <- view _ignores

  when
    (desired ^. _owner /= actual ^. _owner)
    (tell [Action'Chown{user = desired ^. _owner . _user, group = desired ^. _owner . _group, path = SomePath path}])
  when
    (desired ^. _mode /= actual ^. _mode)
    (tell [Action'Chmod{mode = desired ^. _mode, path = SomePath path}])

  case (desired ^. _content, actual ^. _content) of
    (managed'desired@DirectoryContentManaged{}, managed'actual@DirectoryContentManaged{}) -> do
      hashmapCompare
        ignores
        (managed'actual ^. _files)
        (managed'desired ^. _files)
        (createFile path)
        (modifyFile path)
        (deleteFile path)

      hashmapCompare
        ignores
        (managed'actual ^. _directories)
        (managed'desired ^. _directories)
        (createDirectory path)
        (modifyDirectory . (path </>))
        (deleteDirectory path)

      hashmapCompare
        ignores
        (managed'actual ^. _links)
        (managed'desired ^. _links)
        (createLink path)
        (modifyLink path)
        (deleteLink path)
    (DirectoryContentUnmanaged, DirectoryContentUnmanaged) -> pure ()
    _ ->
      logWarnN
        ("Mismatch between directory contents for directory " <> T.show path <> " this is a bug, please open a bug report.")

io :: (MonadIO m) => IO a -> m a
io = liftIO

data FileHammerException = InvalidSpecification String
  deriving (Show)
instance Exception FileHammerException

readSpecification :: (MonadIO m, MonadThrow m) => Path Abs File -> m Specification
readSpecification path =
  (io . A.eitherDecodeFileStrict . toFilePath $ path) >>= \case
    Left err -> throwM $ InvalidSpecification err
    Right specification -> pure specification

createDirectoryRecursive :: (MonadIO m) => Path Abs Dir -> FileMode -> m ()
createDirectoryRecursive path mode = forM_ (map (toFilePath . foldl (</>) [absdir|/|]) . inits . reverse $ segmentPath path) \superPath -> do
  directoryExists <- io $ fileExist superPath &&^ (getFileStatus superPath <&> isDirectory)
  when (not directoryExists) . io $ Posix.createDirectory superPath mode
  pure ()
 where
  segmentPath :: Path Abs Dir -> [Path Rel Dir]
  segmentPath path' =
    if path' == [absdir|/|]
      then []
      else dirname path' : segmentPath (Posix.parent path')

cli :: Cli -> LoggingT IO ()
cli (Cli{root, command = Cli.CommandShow{ignores, unmanaged}}) = do
  directoryNode <- (`runReaderT` (FilterInfo{ignores, unmanaged, root})) $ readDir [reldir|.|]

  io $ TL.putStrLn (A.encodeToLazyText Specification{directory = directoryNode, ignores = ignores})
cli (Cli{root, diff, command = Cli.CommandApply{configuration, parent}}) = do
  specification <- readSpecification configuration

  ( `runReaderT`
      FilterInfo
        { ignores = specification ^. _ignores
        , unmanaged = getUnmanaged (specification ^. _directory)
        , root
        }
    )
    do
      when parent $ createDirectoryRecursive root 0o775
      actual <- readDir [reldir|.|]

      when diff $
        io . putStrLn . render . prettyEditExprCompact $
          ediff (specification ^. _directory) actual

      (_, actions) <- runWriterT $ modifyDirectory root (specification ^. _directory, actual)
      io . forM_ actions $ runAction
cli (Cli{root, diff, command = Cli.CommandPlan{configuration, showContents}}) = do
  specification <- readSpecification configuration

  ( `runReaderT`
      FilterInfo
        { ignores = specification ^. _ignores
        , unmanaged = getUnmanaged (specification ^. _directory)
        , root
        }
    )
    do
      actual <- readDir [reldir|.|]

      when diff $
        io . putStrLn . render . prettyEditExprCompact $
          ediff (specification ^. _directory) actual

      (_, actions) <- runWriterT $ modifyDirectory root (specification ^. _directory, actual)
      forM_ actions (io . T.putStrLn . commandToText showContents . actionToCommand)

main :: IO ()
main = do
  cliArgs <- io parseCli

  runStderrLoggingT . filterLogger (const (>= cliArgs ^. _logLevel)) $ cli cliArgs
