-- SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
--
-- SPDX-License-Identifier: MPL-2.0

module SomePath (SomePath (..)) where

import Data.Hashable (Hashable (hashWithSalt))
import Path.Posix (Abs, Path, Rel, toFilePath)

data SomePath b = forall t. SomePath (Path b t)

instance Show (SomePath Abs) where
  show (SomePath path) = show path

instance Show (SomePath Rel) where
  show (SomePath path) = show path

instance Eq (SomePath Rel) where
  (SomePath a) == (SomePath b) = toFilePath a == toFilePath b

instance Hashable (SomePath Rel) where
  hashWithSalt salt (SomePath path) = hashWithSalt salt path
