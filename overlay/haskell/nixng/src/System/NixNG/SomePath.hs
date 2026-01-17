module System.NixNG.SomePath (SomePath (..), pathAbsFile, pathAbsDir, somePathRelM) where

import Data.Bifunctor (Bifunctor (first))
import Data.Hashable (Hashable (hashWithSalt))
import Options.Applicative.Builder (ReadM, eitherReader)
import Path.Posix (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, toFilePath)

pathAbsFile :: ReadM (Path Abs File)
pathAbsFile = eitherReader (first Prelude.show . parseAbsFile)

pathAbsDir :: ReadM (Path Abs Dir)
pathAbsDir = eitherReader (first Prelude.show . parseAbsDir)

somePathRelM :: ReadM (SomePath Rel)
somePathRelM = eitherReader \arg -> go parseRelFile arg <> go parseRelDir arg
 where
  go fn path = case fn path of
    Left err -> Left (Prelude.show err)
    Right res -> Right (SomePath res)

data SomePath b = forall t. SomePath (Path b t)

instance Show (SomePath Abs) where
  show (SomePath path) = show path

instance Show (SomePath Rel) where
  show (SomePath path) = show path

instance Eq (SomePath Rel) where
  (SomePath a) == (SomePath b) = toFilePath a == toFilePath b

instance Hashable (SomePath Rel) where
  hashWithSalt salt (SomePath path) = hashWithSalt salt path
