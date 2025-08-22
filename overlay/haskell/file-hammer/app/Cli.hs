module Cli where

import Options.Applicative hiding (command)
import Options.Applicative qualified as O
import Path
import Prelude hiding (show)
import Prelude qualified

pathAbsFile :: ReadM (Path Abs File)
pathAbsFile = eitherReader \arg -> case parseAbsFile arg of
  Left err -> Left (Prelude.show err)
  Right res -> Right res

pathAbsDir :: ReadM (Path Abs Dir)
pathAbsDir = eitherReader \arg -> case parseAbsDir arg of
  Left err -> Left (Prelude.show err)
  Right res -> Right res

data Command
  = CommandShow
  | CommandApply
      { configuration :: Path Abs File
      }
  | CommandPlan
      { configuration :: Path Abs File
      }

data Cli
  = Cli
  { root :: Path Abs Dir
  , command :: Command
  }

show :: Parser Command
show = pure CommandShow

apply :: Parser Command
apply = CommandApply <$> option pathAbsFile (long "configuration" <> short 'c')

plan :: Parser Command
plan = CommandPlan <$> option pathAbsFile (long "configuration" <> short 'c')

command :: Parser Command
command =
  subparser
    ( O.command "show" (info show (progDesc "Show current configuration"))
        <> O.command "apply" (info apply (progDesc "Apply configuration from file"))
        <> O.command "plan" (info plan (progDesc "Plan the changes of configurations from file"))
    )

cli :: Parser Cli
cli =
  Cli
    <$> option pathAbsDir (long "root" <> short 'r')
    <*> command

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
