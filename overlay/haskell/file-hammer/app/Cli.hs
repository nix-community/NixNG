{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli (Cli (..), parseCli, _logLevel, Command (..)) where

import Control.Monad.Logger.CallStack (LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn))
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Lens.Micro.TH (makeLensesWith)
import Options.Applicative (
  Alternative (many),
  Parser,
  ReadM,
  eitherReader,
  execParser,
  fullDesc,
  header,
  helper,
  info,
  long,
  option,
  optional,
  progDesc,
  short,
  subparser,
  switch,
  (<**>),
 )
import Options.Applicative qualified as O
import Orphans ()
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  parseAbsDir,
  parseAbsFile,
  parseRelDir,
  parseRelFile,
 )
import System.FilePath.Glob (Pattern, compile)
import System.NixNG.SomePath (SomePath (SomePath), pathAbsDir, pathAbsFile, somePathRelM)
import System.NixNG.TH (duplicateRules)

logLevelM :: ReadM LogLevel
logLevelM = eitherReader \arg ->
  case map toLower arg of
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "error" -> Right LevelError
    _ -> Left ("Invalid log level: " <> Prelude.show arg)

patternM :: ReadM Pattern
patternM = eitherReader \arg -> Right $ compile arg

data Command
  = CommandShow
      { ignores :: HashSet Pattern
      , unmanaged :: HashSet (SomePath Rel)
      }
  | CommandApply
      { configuration :: Path Abs File
      , parent :: Bool
      }
  | CommandPlan
      { configuration :: Path Abs File
      , showContents :: Bool
      }

data Cli
  = Cli
  { root :: Path Abs Dir
  , diff :: Bool
  , command :: Command
  , logLevel :: LogLevel
  }
makeLensesWith duplicateRules ''Cli

showCommand :: Parser Command
showCommand =
  CommandShow
    <$> (many (option patternM (long "ignore" <> short 'i')) <&> HS.fromList)
    <*> (many (option somePathRelM (long "unmanaged" <> short 'u')) <&> HS.fromList)

applyCommand :: Parser Command
applyCommand =
  CommandApply
    <$> option pathAbsFile (long "configuration" <> short 'c')
    <*> switch (long "parent" <> short 'p')

planCommand :: Parser Command
planCommand =
  CommandPlan
    <$> option pathAbsFile (long "configuration" <> short 'c')
    <*> switch (long "show-contents" <> short 'C')

parseCommand :: Parser Command
parseCommand =
  subparser
    ( O.command "show" (info showCommand (progDesc "Show current configuration"))
        <> O.command "apply" (info applyCommand (progDesc "Apply configuration from file"))
        <> O.command "plan" (info planCommand (progDesc "Plan the changes of configurations from file"))
    )

cli :: Parser Cli
cli =
  Cli
    <$> option pathAbsDir (long "root" <> short 'r')
    <*> switch (long "diff" <> short 'd')
    <*> parseCommand
    <*> (optional (option logLevelM (long "log-level" <> short 'L')) <&> maybe LevelInfo id)

parseCli :: IO Cli
parseCli = execParser opts
 where
  opts =
    info
      (cli <**> helper)
      ( fullDesc
          <> progDesc "Hammer a filesystem into shape!"
          <> header "file-hammer - a hammer for files"
      )
