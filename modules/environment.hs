{-# LANGUAGE ImportQualifiedPost, DeriveAnyClass, BlockArguments, ViewPatterns #-}

module Main where

import Data.Aeson qualified as A
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Data.HashMap.Strict as HMS
import Data.HashMap.Strict (HashMap)
import System.Exit (ExitCode(..))
import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing)
import System.Posix.Files (createSymbolicLink)
import System.Exit (exitWith)
import System.FilePath.Posix (takeDirectory, (</>))

data EtcEntry
  = EtcEntry
  { enable :: Bool
  , target :: String
  , source :: String
  , mode :: String
  , user :: String
  , group :: String
  , uid :: String
  , gid :: String
  }
  deriving (Show, Generic, A.FromJSON)

etcDirectory :: FilePath
etcDirectory = "/etc"

main :: IO ()
main = do
  args <- getArgs
  let
    etcFile = args !! 0
  etcs' :: Maybe (HashMap String EtcEntry) <- A.decodeFileStrict etcFile

  case etcs' of
    Nothing -> do
      putStrLn $ "Unable to deserialize " <> etcFile
      exitWith (ExitFailure 1)
    Just (HMS.elems -> etcs) -> forM_ etcs \etc ->
      let
        subdir = takeDirectory (target etc)
        exactTarget = etcDirectory </> (target etc)
      in
        if mode etc == "symlink" then do
          createDirectoryIfMissing True (etcDirectory </> subdir)
          createSymbolicLink (source etc) exactTarget
        else do
          putStrLn $ "Other modes than symlink are not supported"
