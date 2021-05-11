{-# LANGUAGE DeriveDataTypeable #-}

module CmdLine where

import System.Console.CmdArgs

data CmdLine = Sample {cmdLineConfigFile :: FilePath}
  deriving (Show, Data, Typeable)

cmdline = Sample { cmdLineConfigFile = def &= args &= typFile }
