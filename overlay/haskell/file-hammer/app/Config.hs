{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import ByteString.Aeson.Orphans ()
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable (..))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.TreeDiff (Expr (App), ToExpr (..))
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLensesWith)
import Orphans
import Path
import SomePath (SomePath (..))
import System.FilePath.Glob (Pattern, compile, decompile)
import System.Posix.Types (CMode (..))
import TH

data Owner
  = Owner
  { user :: Text
  , group :: Text
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data Content
  = ContentAny
  | ContentText Text
  | ContentBinary ByteString
  | ContentFile (Path Abs File)
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data FileNode
  = FileNode
  { owner :: Owner
  , mode :: CMode
  , content :: Content
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data DirectoryContent
  = DirectoryContentManaged
      { file :: HashMap (Path Rel File) FileNode
      , link :: HashMap (Path Rel File) LinkNode
      , directories :: HashMap (Path Rel Dir) DirectoryNode
      }
  | DirectoryContentUnmanaged
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data DirectoryNode
  = DirectoryNode
  { owner :: Owner
  , mode :: CMode
  , content :: DirectoryContent
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data LinkNode
  = LinkNode
  { destination :: Text
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data Specification
  = Specification
  { ignores :: HashSet Pattern
  , directory :: DirectoryNode
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show)

makeLensesWith duplicateRules ''Owner
makeLensesWith duplicateRules ''Content
makeLensesWith duplicateRules ''FileNode
makeLensesWith duplicateRules ''DirectoryContent
makeLensesWith duplicateRules ''DirectoryNode
makeLensesWith duplicateRules ''LinkNode
makeLensesWith duplicateRules ''Specification

getUnmanaged :: DirectoryNode -> HashSet (SomePath Rel)
getUnmanaged DirectoryNode{content = DirectoryContentUnmanaged} = HS.empty
getUnmanaged DirectoryNode{content = DirectoryContentManaged{directories = directories'}} =
  foldl HS.union HS.empty $ map (uncurry getUnmanaged') (HM.toList directories')
 where
  getUnmanaged' :: (Path Rel Dir) -> DirectoryNode -> HashSet (SomePath Rel)
  getUnmanaged' path DirectoryNode{content = DirectoryContentUnmanaged} =
    HS.singleton (SomePath path)
  getUnmanaged' path DirectoryNode{content = DirectoryContentManaged{directories = directories'', file = file'}} =
    fileUnmanaged `HS.union` dirUnmanaged
   where
    fileUnmanaged = HS.fromList . catMaybes $ (`map` HM.toList file') \(name, oneFile) ->
      case oneFile ^. _content of
        ContentAny -> Just . SomePath $ path </> name
        _ -> Nothing
    dirUnmanaged =
      foldl HS.union HS.empty
        . map (\(name, next) -> getUnmanaged' (path </> name) next)
        $ (HM.toList directories'')
