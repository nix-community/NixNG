-- SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE DeriveDataTypeable #-}

module CmdLine where

import System.Console.CmdArgs

data CmdLine = Sample {cmdLineConfigFile :: FilePath}
  deriving (Show, Data, Typeable)

cmdline = Sample { cmdLineConfigFile = def &= args &= typFile }
