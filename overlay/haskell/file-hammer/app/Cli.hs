{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli where

import Control.Monad.Logger (LogLevel (..))
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Lens.Micro.TH (makeLensesWith)
import Options.Applicative hiding (command)
import Options.Applicative qualified as O
import Orphans ()
import Path
import SomePath (SomePath (..))
import System.FilePath.Glob (Pattern, compile)
import TH
import Prelude hiding (show)
import Prelude qualified

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

pathRelDir :: ReadM (Path Rel Dir)
pathRelDir = eitherReader (first Prelude.show . parseRelDir)

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

show :: Parser Command
show =
  CommandShow
    <$> (many (option patternM (long "ignore" <> short 'i')) <&> HS.fromList)
    <*> (many (option somePathRelM (long "unmanaged" <> short 'u')) <&> HS.fromList)

apply :: Parser Command
apply =
  CommandApply
    <$> option pathAbsFile (long "configuration" <> short 'c')
    <*> switch (long "parent" <> short 'p')

plan :: Parser Command
plan =
  CommandPlan
    <$> option pathAbsFile (long "configuration" <> short 'c')
    <*> switch (long "show-contents" <> short 'C')

parseCommand :: Parser Command
parseCommand =
  subparser
    ( O.command "show" (info show (progDesc "Show current configuration"))
        <> O.command "apply" (info apply (progDesc "Apply configuration from file"))
        <> O.command "plan" (info plan (progDesc "Plan the changes of configurations from file"))
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
