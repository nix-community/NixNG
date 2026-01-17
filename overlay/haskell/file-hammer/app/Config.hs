{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (
  getUnmanaged,
  Content (..),
  Group (..),
  User (..),
  DirectoryContent (..),
  DirectoryNode (..),
  FileNode (..),
  LinkNode (..),
  Owner (..),
  Specification (..),
  HasIgnores (),
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
) where

import ByteString.Aeson.Orphans ()
import Data.Aeson qualified as A
import Data.Aeson.Types (
  SumEncoding (ObjectWithSingleField),
  defaultOptions,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
  sumEncoding,
 )
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLensesWith)
import Orphans ()
import Path (Abs, Dir, File, Path, Rel, (</>))
import SomePath (SomePath (SomePath))
import System.FilePath.Glob (Pattern)
import System.Posix.Types (CMode, GroupID, UserID)
import TH (duplicateRules)

customOptions :: A.Options
customOptions =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    }

{- |
  A user described either by their name or numeric ID
-}
data User = UserName Text | UserId UserID
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON User where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON User where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  A group described either by their name or numeric ID
-}
data Group = GroupName Text | GroupId GroupID
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON Group where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON Group where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  Owner specification of a filesystem node, containing the ownning
  user and group.
-}
data Owner
  = Owner
  { user :: User
  -- ^ The owning user of a filesystem node.
  , group :: Group
  -- ^ The owning group of a filesystem node.
  }
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON Owner where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON Owner where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  The content of a file, one of:
    - "ContentAny":
      The file will be created but it's content left unmanaged (initially empty).
    - "ContentText":
      The files content will be set to the provided text.
    - "ContentBinary":
      The files content will be set to the provided base64-encoded binary data.
    - "ContentFile":
      The files content will be set to the content of the file specificied. The second file
      is read at application/plan time.
-}
data Content
  = -- | Leave this file's content unmanaged.
    ContentAny
  | -- | Set this file's content to the specified text.
    ContentText Text
  | -- | Set this file's content to the specified base64-encoded binary data.
    ContentBinary ByteString
  | -- | Set this file's content to the content of another file.
    ContentFile (Path Abs File)
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON Content where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON Content where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  A description of a file with its owner, mode and content information.
-}
data FileNode
  = FileNode
  { owner :: Owner
  -- ^ The owner information of this file node.
  , mode :: CMode
  -- ^ The mode of this file node.
  , content :: Content
  -- ^ The content of this file node.
  }
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON FileNode where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON FileNode where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  The content of a directory, one of:
    - "DirectoryContentManaged":
      The directory's content will be managed and ensured to consist of the files, links
      and directories specified
    - "DirectoryContentUnmanaged":
      The directory's content will be left unmanaged, the directory will be created initially
      empty.
-}
data DirectoryContent
  = -- | Manage this directory's contents.
    DirectoryContentManaged
      { files :: HashMap (Path Rel File) FileNode
      -- ^ The files that are to exist in this directory's contents.
      , links :: HashMap (Path Rel File) LinkNode
      -- ^ The links that are to exist in this directory's contents.
      , directories :: HashMap (Path Rel Dir) DirectoryNode
      -- ^ The directories that are to exist in this directory's contents.
      }
  | -- | Don't manage this directory's contents, but only ensure it's existence.
    DirectoryContentUnmanaged
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON DirectoryContent where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON DirectoryContent where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  A description of a directory with its owner, mode and content information.
-}
data DirectoryNode
  = DirectoryNode
  { owner :: Owner
  -- ^ The owner information of this directory node.
  , mode :: CMode
  -- ^ The mode of this directory node.
  , content :: DirectoryContent
  -- ^ The content of this directory node.
  }
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON DirectoryNode where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON DirectoryNode where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  A description of a link with just its destination.
-}
data LinkNode
  = LinkNode
  { destination :: Text
  -- ^ The destination of this link node.
  }
  deriving (Eq, Generic, Show, ToExpr)

instance A.FromJSON LinkNode where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON LinkNode where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

{- |
  A specification which will be applied by the `file-hammer` upon an `apply`.
  `ignores` specifies a list of glob patterns to completely ignore.

  WARNING:
    `file-hammer` will still delete ignored files if they are contained in a directory
    which itself is to be deleted.
-}
data Specification
  = Specification
  { ignores :: HashSet Pattern
  -- ^ Any nodes that are to be completely ignored. See warning for the `specification` data type.
  , directory :: DirectoryNode
  -- ^ The starting directory of this specification.
  }
  deriving (Eq, Generic, Show)

instance A.FromJSON Specification where
  parseJSON = genericParseJSON customOptions
instance A.ToJSON Specification where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

makeLensesWith duplicateRules ''User
makeLensesWith duplicateRules ''Group
makeLensesWith duplicateRules ''Owner
makeLensesWith duplicateRules ''Content
makeLensesWith duplicateRules ''FileNode
makeLensesWith duplicateRules ''DirectoryContent
makeLensesWith duplicateRules ''DirectoryNode
makeLensesWith duplicateRules ''LinkNode
makeLensesWith duplicateRules ''Specification

getUnmanaged :: DirectoryNode -> HashSet (SomePath Rel)
getUnmanaged DirectoryNode{content = DirectoryContentUnmanaged} = HS.empty
getUnmanaged DirectoryNode{content = DirectoryContentManaged{directories}} =
  foldl HS.union HS.empty $ map (uncurry getUnmanaged') (HM.toList directories)
 where
  getUnmanaged' :: (Path Rel Dir) -> DirectoryNode -> HashSet (SomePath Rel)
  getUnmanaged' path DirectoryNode{content = DirectoryContentUnmanaged} =
    HS.singleton (SomePath path)
  getUnmanaged' path DirectoryNode{content = DirectoryContentManaged{directories = directories', files = file'}} =
    fileUnmanaged `HS.union` dirUnmanaged
   where
    fileUnmanaged = HS.fromList . catMaybes $ (`map` HM.toList file') \(name, oneFile) ->
      case oneFile ^. _content of
        ContentAny -> Just . SomePath $ path </> name
        _ -> Nothing
    dirUnmanaged =
      foldl HS.union HS.empty
        . map (\(name, next) -> getUnmanaged' (path </> name) next)
        $ (HM.toList directories')
