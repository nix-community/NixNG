{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Config where

import ByteString.Aeson.Orphans ()
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text qualified as T
import Data.TreeDiff (Expr (App), ToExpr (..))
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLensesWith)
import Path
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

deriving newtype instance A.FromJSON CMode
deriving newtype instance A.ToJSON CMode
deriving newtype instance ToExpr CMode

data FileNode
  = FileNode
  { owner :: Owner
  , mode :: CMode
  , content :: Content
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data DirectoryNode
  = DirectoryNode
  { ignoreGlobs :: [Pattern]
  , owner :: Owner
  , mode :: CMode
  , file :: HashMap (Path Rel File) FileNode
  , link :: HashMap (Path Rel File) LinkNode
  , directory :: HashMap (Path Rel Dir) DirectoryNode
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

data LinkNode
  = LinkNode
  { destination :: Text
  }
  deriving (A.FromJSON, A.ToJSON, Eq, Generic, Show, ToExpr)

instance ToExpr (Path p t) where
  toExpr :: Path p t -> Expr
  toExpr path = App (toFilePath path) []

instance ToExpr Pattern where
  toExpr :: Pattern -> Expr
  toExpr pat = App (show pat) []

instance A.ToJSON Pattern where
  toJSON :: Pattern -> A.Value
  toJSON pat = A.String . T.pack $ decompile pat

instance A.FromJSON Pattern where
  parseJSON :: A.Value -> A.Parser Pattern
  parseJSON = A.withText "Pattern" (pure . compile . T.unpack)

makeLensesWith duplicateRules ''Owner
makeLensesWith duplicateRules ''Content
makeLensesWith duplicateRules ''FileNode
makeLensesWith duplicateRules ''DirectoryNode
makeLensesWith duplicateRules ''LinkNode
