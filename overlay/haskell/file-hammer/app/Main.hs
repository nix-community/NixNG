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
  group,
  ignoreExtra,
  link,
  mode,
  owner,
  user,
 )
import Control.Monad (forM_, when)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class
import Control.Monad.State.Strict (StateT, execStateT)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (intersperse)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.TreeDiff
import Lens.Micro
import Lens.Micro.Mtl
import Path.Posix
import System.Directory hiding (createDirectory, isSymbolicLink)
import System.Posix.Files hiding (createLink)
import System.Posix.User
import Text.PrettyPrint (render)

getHashMaps
  :: Path Abs Dir
  -> [FilePath]
  -> StateT (HashMap (Path Rel File) FileNode, HashMap (Path Rel Dir) DirectoryNode, HashMap (Path Rel File) LinkNode) IO ()
getHashMaps root paths =
  forM_ paths \((toFilePath root <>) -> ent) -> do
    status <- io $ getSymbolicLinkStatus ent

    if
      | isRegularFile status -> do
          (path, absPath) <-
            parseSomeFile ent <&> \case
              Abs abs' -> (filename abs', abs')
              Rel rel -> (rel, root </> rel)

          fileNode <- liftIO $ readFileNode absPath

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

          dirNode <- liftIO $ readDir absPath

          _2
            %= HM.insert
              path
              dirNode
      | otherwise -> liftIO $ putStrLn "not anything?"

readContent :: Path Abs File -> IO Content
readContent path =
  BS.readFile (fromAbsFile path) <&> \bs -> case T.decodeUtf8' bs of
    Left unicodeException -> ContentBinary bs
    Right text -> ContentText text

readFileNode :: Path Abs File -> IO FileNode
readFileNode path = do
  status <- getFileStatus (fromAbsFile path)

  userEntry <- getUserEntryForID (fileOwner status)
  groupEntry <- getGroupEntryForID (fileGroup status)

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

readDir :: Path Abs Dir -> IO DirectoryNode
readDir path = do
  (files, directories, links) <-
    listDirectory (fromAbsDir path) >>= (`execStateT` (HM.empty, HM.empty, HM.empty)) . getHashMaps path
  status <- getFileStatus (fromAbsDir path)

  userEntry <- getUserEntryForID (fileOwner status)
  groupEntry <- getGroupEntryForID (fileGroup status)

  pure
    DirectoryNode
      { ignoreExtra = True
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

modifyFile :: Path Abs Dir -> Path Rel File -> (FileNode, FileNode) -> WriterT [Action] IO ()
modifyFile root name (desired, actual) = do
  when (desired ^. owner /= actual ^. owner) $
    tell [Action'Chown{path = SomePath $ root </> name, user = desired ^. owner . user, group = desired ^. owner . group}]
  when (desired ^. mode /= actual ^. mode) $
    tell [Action'Chmod{path = SomePath $ root </> name, mode = desired ^. mode}]

  case (desired ^. content, actual ^. content) of
    (ContentFile path, actualContent) -> do
      desiredContent <- io $ readContent path
      when (desiredContent /= actualContent) $
        tell [Action'UpdateFile{filePath = root </> name, content = desired ^. content}]
    _ ->
      when (desired ^. content /= actual ^. content) $
        tell [Action'UpdateFile{filePath = root </> name, content = desired ^. content}]

createFile :: Path Abs Dir -> Path Rel File -> FileNode -> WriterT [Action] IO ()
createFile root name desired =
  tell
    [ Action'CreateFile
        { filePath = root </> name
        , content = desired ^. content
        , user = desired ^. owner . user
        , group = desired ^. owner . group
        , mode = desired ^. mode
        }
    ]

deleteFile :: Path Abs Dir -> Path Rel File -> FileNode -> WriterT [Action] IO ()
deleteFile root name _ =
  tell [Action'DeleteFile{filePath = root </> name}]

createDirectory :: Path Abs Dir -> DirectoryNode -> WriterT [Action] IO ()
createDirectory path desired = do
  tell
    [ Action'CreateDirectory
        { dirPath = path
        , user = desired ^. owner . user
        , group = desired ^. owner . group
        , mode = desired ^. mode
        }
    ]

  forM_ (HM.toList $ desired ^. file) . uncurry $ createFile path
  forM_ (HM.toList $ desired ^. directory) . uncurry $ createDirectory . (path </>)

deleteDirectory :: Path Abs Dir -> DirectoryNode -> WriterT [Action] IO ()
deleteDirectory path _actual =
  tell [Action'DeleteDirectory{dirPath = path}]

createLink :: Path Abs File -> LinkNode -> WriterT [Action] IO ()
createLink path desired = tell [Action'CreateLink{source = path, destination = desired ^. destination}]

modifyLink :: Path Abs File -> (LinkNode, LinkNode) -> WriterT [Action] IO ()
modifyLink path (desired, _actual) = tell [Action'UpdateLink{source = path, destination = desired ^. destination}]

deleteLink :: Path Abs File -> LinkNode -> WriterT [Action] IO ()
deleteLink path _actual = tell [Action'DeleteLink{source = path}]

hashmapCompare
  :: ( Eq v
     , Hashable k
     , Monad m
     )
  => HashMap k v
  -> HashMap k v
  -> (k -> v -> m ())
  -> (k -> (v, v) -> m ())
  -> Bool
  -> (k -> v -> m ())
  -> m ()
hashmapCompare first second create modify deleteGuard delete = do
  let
    deleted = first `HM.difference` second
    created = second `HM.difference` first
    updated = HM.intersectionWith maybeNotEqual second first & HM.mapMaybe id

  forM_ (HM.toList created) . uncurry $ create
  forM_ (HM.toList updated) . uncurry $ modify
  when deleteGuard $ forM_ (HM.toList deleted) . uncurry $ delete
 where
  maybeNotEqual a b = if a /= b then Just (a, b) else Nothing

modifyDirectory :: Path Abs Dir -> (DirectoryNode, DirectoryNode) -> WriterT [Action] IO ()
modifyDirectory path (desired, actual) = do
  when
    (desired ^. owner /= actual ^. owner)
    (tell [Action'Chown{user = desired ^. owner . user, group = desired ^. owner . group, path = SomePath path}])
  when
    (desired ^. mode /= actual ^. mode)
    (tell [Action'Chmod{mode = desired ^. mode, path = SomePath path}])

  hashmapCompare
    (actual ^. file)
    (desired ^. file)
    (createFile path)
    (modifyFile path)
    (not $ desired ^. ignoreExtra)
    (deleteFile path)

  hashmapCompare
    (actual ^. directory)
    (desired ^. directory)
    (createDirectory . (path </>))
    (modifyDirectory . (path </>))
    (not $ desired ^. ignoreExtra)
    (deleteDirectory . (path </>))

  hashmapCompare
    (actual ^. link)
    (desired ^. link)
    (createLink . (path </>))
    (modifyLink . (path </>))
    (not $ desired ^. ignoreExtra)
    (deleteLink . (path </>))

-- deleteFile :: Path Abs File -> FileNode -> WriterT [Action] IO ()
-- deleteFile (toFilePath -> path) _ = io $ Unix.removeLink path

-- writeContent :: Content -> Fd -> IO ()
-- writeContent ContentAny _ = pure ()
-- writeContent (ContentBinary bs) fd = io $ Unix.fdToHandle fd >>= (`BS.hPut` bs)
-- writeContent (ContentText text) fd = io $ Unix.fdToHandle fd >>= (`T.hPutStr` text)

-- updateFile :: Path Abs File -> (FileNode, FileNode) -> WriterT [Action] IO ()
-- updateFile _ (FileNode{content = ContentAny}, _) = pure ()
-- updateFile (toFilePath -> path) (desired, actual) =
--   void $
--     generalBracket
--       (io $ Unix.openFd path Unix.WriteOnly openFlags)
--       ( \fd exitCase ->
--           io $
--             Unix.closeFd fd <* case exitCase of
--               ExitCaseException _ -> writeContent (actual ^. content) fd
--               ExitCaseAbort -> writeContent (actual ^. content) fd
--               ExitCaseSuccess _ -> pure ()
--       )
--       (io . writeContent (desired ^. content))
--  where
--   openFlags = Unix.defaultFileFlags{Unix.trunc = True, Unix.creat = Just $ desired ^. mode}

-- createFile :: Path Abs File -> FileNode -> WriterT [Action] IO ()
-- createFile (toFilePath -> path) file' = do
--   void $
--     generalBracket
--       (io $ Unix.createFile path (file' ^. mode))
--       ( \fd exitCase ->
--           io $
--             Unix.closeFd fd <* case exitCase of
--               ExitCaseException _ -> Unix.removeLink path
--               ExitCaseAbort -> Unix.removeLink path
--               ExitCaseSuccess _ -> Unix.closeFd fd
--       )
--       ( \fd -> do
--           case file' ^. content of
--             ContentAny -> pure ()
--             ContentText text -> io $ Unix.fdToHandle fd >>= (`T.hPutStr` text)
--             ContentBinary bytestring -> io $ Unix.fdToHandle fd >>= (`BS.hPut` bytestring)
--       )

io :: (MonadIO m) => IO a -> m a
io = liftIO

cli :: Cli -> IO ()
cli (Cli{root, command = Cli.CommandShow}) = do
  io (readDir root) <&> A.encodeToLazyText >>= io . TL.putStrLn
cli (Cli{root, diff, command = Cli.CommandApply{configuration = desired'}}) = do
  actual <- io $ readDir root
  io (A.eitherDecodeFileStrict (toFilePath desired')) >>= \case
    Left err -> io $ putStrLn err
    Right desired -> do
      when diff $
        putStrLn . render . prettyEditExprCompact $
          ediff desired actual

      (_, actions) <- runWriterT $ modifyDirectory root (desired, actual)
      forM_ actions runAction
cli (Cli{root, diff, command = Cli.CommandPlan{configuration = desired'}}) = do
  actual <- io $ readDir root
  io (A.eitherDecodeFileStrict (toFilePath desired')) >>= \case
    Left err -> io $ putStrLn err
    Right desired -> do
      when diff $
        putStrLn . render . prettyEditExprCompact $
          ediff desired actual

      (_, actions) <- runWriterT $ modifyDirectory root (desired, actual)
      forM_ actions (T.putStrLn . commandToText True . actionToCommand)

main :: IO ()
main = cli =<< parseCli
