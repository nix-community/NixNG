{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Data.Aeson.Types qualified as A
import Data.Hashable (Hashable (..))
import Data.Text qualified as T
import Data.TreeDiff.Class (ToExpr (..))
import Data.TreeDiff.Expr (Expr (..))
import Path.Posix (Path, toFilePath)
import System.FilePath.Glob (Pattern, compile, decompile)
import System.Posix.Types (CMode (..))

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

instance Hashable Pattern where
  hashWithSalt :: Int -> Pattern -> Int
  hashWithSalt salt a = hashWithSalt salt $ T.show a

deriving newtype instance A.FromJSON CMode
deriving newtype instance A.ToJSON CMode
deriving newtype instance ToExpr CMode
