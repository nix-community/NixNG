-- SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans () where

import Data.Aeson.Types qualified as A
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text qualified as T
import Data.TreeDiff.Class (ToExpr (toExpr))
import Data.TreeDiff.Expr (Expr (App))
import Path.Posix (Path, toFilePath)
import System.FilePath.Glob (Pattern, compile, decompile)
import System.Posix.Types (CGid (CGid), CMode (CMode), CUid (CUid), GroupID, UserID)

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

deriving newtype instance ToExpr UserID
deriving newtype instance A.FromJSON UserID
deriving newtype instance A.ToJSON UserID

deriving newtype instance ToExpr GroupID
deriving newtype instance A.FromJSON GroupID
deriving newtype instance A.ToJSON GroupID
