{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Action (Action (..), Action'Show (..), SomePath (..), actionToCommand, commandToText, runAction)
import ByteString.Aeson.Orphans ()
import Cli (Cli (..), parseCli)
import Cli qualified
import Config (
  Content (..),
  DirectoryNode (..),
  FileNode (..),
  LinkNode (..),
  Owner (..),
  content,
  destination,
  directory,
  file,
  getGlobs,
  group,
  ignoreGlobs,
  link,
  mode,
  owner,
  user,
 )
import Control.Monad (forM_, when)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Logger.CallStack (LoggingT, MonadLoggerIO, logDebugN)
import Control.Monad.State.Strict (StateT, execStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (intersperse, isPrefixOf, (\\))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.TreeDiff
import Lens.Micro
import Lens.Micro.Mtl
import Path.Posix
import System.Directory hiding (createDirectory, isSymbolicLink)
import System.FilePath.Glob (Pattern, compile, decompile, match)
import System.Posix.Files hiding (createLink)
import System.Posix.User
import Text.PrettyPrint (render)

type AppM = LoggingT IO

type GetHashMaps =
  ( HashMap (Path Rel File) FileNode
  , HashMap (Path Rel Dir) DirectoryNode
  , HashMap (Path Rel File) LinkNode
  )

getHashMaps
  :: [Pattern]
  -> Path Abs Dir
  -> [FilePath]
  -> StateT
       GetHashMaps
       AppM
       ()
getHashMaps exclusions root paths = do
  let
    filteredPaths = filter (\name -> not $ any (`match` (toFilePath root <> name)) exclusions) paths
    excludedNames = paths \\ filteredPaths

  logDebugN ("excluded names: " <> T.show excludedNames)

  forM_ filteredPaths \((toFilePath root <>) -> ent) -> do
    status <- io $ getSymbolicLinkStatus ent

    if
      | isRegularFile status -> do
          (path, absPath) <-
            parseSomeFile ent <&> \case
              Abs abs' -> (filename abs', abs')
              Rel rel -> (rel, root </> rel)

          fileNode <- lift $ readFileNode absPath

          _1
            %= HM.insert
              path
              fileNode
      | isSymbolicLink status -> do
          path <-
            parseSomeFile ent <&> \case
              Abs abs' -> filename abs'
              Rel rel -> rel
          destination <- io $ readSymbolicLink ent
          _3 %= HM.insert path LinkNode{destination = T.pack destination}
      | isDirectory status -> do
          (path, absPath) <-
            parseSomeDir ent <&> \case
              Abs abs' -> (dirname abs', abs')
              Rel rel -> (rel, root </> rel)

          dirNode <- lift $ readDir exclusions absPath

          _2
            %= HM.insert
              path
              dirNode
      | otherwise -> io . putStrLn $ ent <> "is of unknown type"

readContent :: Path Abs File -> AppM Content
readContent path =
  io (BS.readFile (fromAbsFile path)) >>= \bs -> case T.decodeUtf8' bs of
    Left _unicodeException -> do
      logDebugN ("content of " <> T.show path <> " looks binary")
      pure $ ContentBinary bs
    Right text -> pure $ ContentText text

readFileNode :: Path Abs File -> AppM FileNode
readFileNode path = do
  logDebugN ("scraping file " <> T.show path)

  status <- io $ getFileStatus (fromAbsFile path)

  userEntry <- io $ getUserEntryForID (fileOwner status)
  groupEntry <- io $ getGroupEntryForID (fileGroup status)

  fileContent <- readContent path

  pure $
    FileNode
      { owner =
          Owner
            { user = T.pack $ userName userEntry
            , group = T.pack $ groupName groupEntry
            }
      , mode = fileMode status .&. 0b111111111
      , content = fileContent
      }

readDir :: [Pattern] -> Path Abs Dir -> AppM DirectoryNode
readDir exclusions path = do
  let excludesAll = any (\(decompile -> pat) -> toFilePath path `isPrefixOf` pat && toFilePath path <> "*" == pat) exclusions
  (files, directories, links) <-
    if excludesAll
      then do
        logDebugN ("not recursing into directory " <> T.show path <> " due to full exclusion")
        pure (HM.empty, HM.empty, HM.empty)
      else do
        logDebugN ("recursing into directory " <> T.show path)
        io (listDirectory (fromAbsDir path)) >>= (`execStateT` (HM.empty, HM.empty, HM.empty)) . getHashMaps exclusions path

  status <- io $ getFileStatus (fromAbsDir path)

  userEntry <- io $ getUserEntryForID (fileOwner status)
  groupEntry <- io $ getGroupEntryForID (fileGroup status)

  pure
    DirectoryNode
      { ignoreGlobs = []
      , owner =
          Owner
            { user = T.pack $ userName userEntry
            , group = T.pack $ groupName groupEntry
            }
      , mode = fileMode status .&. 0b111111111
      , file = files
      , directory = directories
      , link = links
      }

modifyFile :: Path Abs Dir -> Path Rel File -> (FileNode, FileNode) -> WriterT [Action] AppM ()
modifyFile path name (desired, actual) = do
  when (desired ^. owner /= actual ^. owner) $
    tell [Action'Chown{path = SomePath $ path </> name, user = desired ^. owner . user, group = desired ^. owner . group}]
  when (desired ^. mode /= actual ^. mode) $
    tell [Action'Chmod{path = SomePath $ path </> name, mode = desired ^. mode}]

  case (desired ^. content, actual ^. content) of
    (ContentFile contentPath, actualContent) -> do
      desiredContent <- lift $ readContent contentPath
      when (desiredContent /= actualContent) $
        tell [Action'UpdateFile{filePath = path </> name, content = desired ^. content}]
    _ ->
      when (desired ^. content /= actual ^. content) $
        tell [Action'UpdateFile{filePath = path </> name, content = desired ^. content}]

createFile :: Path Abs Dir -> Path Rel File -> FileNode -> WriterT [Action] AppM ()
createFile path name desired =
  tell
    [ Action'CreateFile
        { filePath = path </> name
        , content = desired ^. content
        , user = desired ^. owner . user
        , group = desired ^. owner . group
        , mode = desired ^. mode
        }
    ]

deleteFile :: Path Abs Dir -> Path Rel File -> FileNode -> WriterT [Action] AppM ()
deleteFile root name _ =
  tell [Action'DeleteFile{filePath = root </> name}]

excludeByName :: [Pattern] -> Path Rel t -> Bool
excludeByName exclusions (toFilePath -> name) = not $ any (`match` name') exclusions
 where
  name' = if last name == '/' then init name else name

createDirectory :: Path Abs Dir -> Path Rel Dir -> DirectoryNode -> WriterT [Action] AppM ()
createDirectory path name desired = do
  let
    path' = path </> name
    filterByExclusions = filter \(name, _) -> excludeByName (desired ^. ignoreGlobs) name

  tell
    [ Action'CreateDirectory
        { dirPath = path'
        , user = desired ^. owner . user
        , group = desired ^. owner . group
        , mode = desired ^. mode
        }
    ]

  forM_ (filterByExclusions . HM.toList $ desired ^. file) . uncurry $ createFile path'
  forM_ (filterByExclusions . HM.toList $ desired ^. directory) . uncurry $ createDirectory path'

deleteDirectory :: Path Abs Dir -> Path Rel Dir -> DirectoryNode -> WriterT [Action] AppM ()
deleteDirectory path name _actual =
  tell [Action'DeleteDirectory{dirPath = path </> name}]

createLink :: Path Abs Dir -> Path Rel File -> LinkNode -> WriterT [Action] AppM ()
createLink path name desired = tell [Action'CreateLink{source = path </> name, destination = desired ^. destination}]

modifyLink :: Path Abs Dir -> Path Rel File -> (LinkNode, LinkNode) -> WriterT [Action] AppM ()
modifyLink path name (desired, _actual) = tell [Action'UpdateLink{source = path </> name, destination = desired ^. destination}]

deleteLink :: Path Abs Dir -> Path Rel File -> LinkNode -> WriterT [Action] AppM ()
deleteLink path name _actual = tell [Action'DeleteLink{source = path </> name}]

hashmapCompare
  :: ( Eq v
     , MonadIO m
     , MonadLoggerIO m
     , Show v
     )
  => [Pattern]
  -> HashMap (Path Rel t) v
  -> HashMap (Path Rel t) v
  -> (Path Rel t -> v -> m ())
  -> (Path Rel t -> (v, v) -> m ())
  -> (Path Rel t -> v -> m ())
  -> m ()
hashmapCompare exclusions first second create modify delete = do
  let
    first' = (`HM.filterWithKey` first) \name _ -> excludeByName exclusions name
    second' = (`HM.filterWithKey` second) \name _ -> excludeByName exclusions name

    excludedNames = (first `HM.difference` first') `HM.union` (second `HM.difference` second')

    deleted = first' `HM.difference` second'
    created = second' `HM.difference` first'
    updated = HM.intersectionWith maybeNotEqual second' first' & HM.mapMaybe id

  logDebugN ("excluded names: " <> T.show excludedNames)

  forM_ (HM.toList created) . uncurry $ create
  forM_ (HM.toList updated) . uncurry $ modify
  forM_ (HM.toList deleted) . uncurry $ delete
 where
  maybeNotEqual a b = if a /= b then Just (a, b) else Nothing

modifyDirectory :: Path Abs Dir -> (DirectoryNode, DirectoryNode) -> WriterT [Action] AppM ()
modifyDirectory path (desired, actual) = do
  when
    (desired ^. owner /= actual ^. owner)
    (tell [Action'Chown{user = desired ^. owner . user, group = desired ^. owner . group, path = SomePath path}])
  when
    (desired ^. mode /= actual ^. mode)
    (tell [Action'Chmod{mode = desired ^. mode, path = SomePath path}])

  hashmapCompare
    (desired ^. ignoreGlobs)
    (actual ^. file)
    (desired ^. file)
    (createFile path)
    (modifyFile path)
    (deleteFile path)

  hashmapCompare
    (desired ^. ignoreGlobs)
    (actual ^. directory)
    (desired ^. directory)
    (createDirectory path)
    (modifyDirectory . (path </>))
    (deleteDirectory path)

  hashmapCompare
    (desired ^. ignoreGlobs)
    (actual ^. link)
    (desired ^. link)
    (createLink path)
    (modifyLink path)
    (deleteLink path)

io :: (MonadIO m) => IO a -> m a
io = liftIO

cli :: Cli -> AppM ()
cli (Cli{root, command = Cli.CommandShow{exclusions}}) = do
  readDir (map (compile . (toFilePath root <>) . decompile) exclusions) root
    <&> A.encodeToLazyText
    >>= io . TL.putStrLn
cli (Cli{root, diff, command = Cli.CommandApply{configuration = desired'}}) = do
  io (A.eitherDecodeFileStrict (toFilePath desired')) >>= \case
    Left err -> io $ putStrLn err
    Right desired -> do
      let exclusions = getGlobs desired
      actual <- readDir (map (compile . (toFilePath root <>) . decompile) exclusions) root

      when diff $
        io . putStrLn . render . prettyEditExprCompact $
          ediff desired actual

      (_, actions) <- runWriterT $ modifyDirectory root (desired, actual)
      forM_ actions (io . runAction)
cli (Cli{root, diff, command = Cli.CommandPlan{configuration = desired'}}) = do
  io (A.eitherDecodeFileStrict (toFilePath desired')) >>= \case
    Left err -> io $ putStrLn err
    Right desired -> do
      let exclusions = getGlobs desired
      actual <- readDir (map (compile . (toFilePath root <>) . decompile) exclusions) root

      when diff $
        io . putStrLn . render . prettyEditExprCompact $
          ediff desired actual

      (_, actions) <- runWriterT $ modifyDirectory root (desired, actual)
      forM_ actions (io . T.putStrLn . commandToText True . actionToCommand)

main :: IO ()
main = runStderrLoggingT $ cli =<< io parseCli
