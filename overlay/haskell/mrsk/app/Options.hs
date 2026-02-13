{-# LANGUAGE LambdaCase #-}

module Options where

import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Path.Posix (Abs, Dir, File, Path, parseAbsFile)
import Remote (SwitchAction (..))
import System.NixNG.SomePath (pathAbsDir, pathAbsFile)

data Command
  = Deploy
      { hosts :: Maybe (NonEmpty Text)
      , flake :: Text
      , sudo :: Bool
      , action :: SwitchAction
      }
  | Far
  | Replay {nixInternalLog :: Maybe (Path Abs File), rate :: Maybe Int}

data Logging
  = Logging'OtherTerminal
  | Logging'File (Path Abs File)
  | Logging'Blackhole
  | Logging'Stderr

data Options = Options
  { command :: Command
  , logging :: Logging
  , nom :: Bool
  , dumpLog :: Maybe (Path Abs File)
  }

readLoggingM :: ReadM Logging
readLoggingM =
  eitherReader \case
    "other-terminal" -> Right Logging'OtherTerminal
    "blackhole" -> Right Logging'Blackhole
    "stderr" -> Right Logging'Stderr
    path'
      | "file:" `isPrefixOf` path' ->
          case parseAbsFile (drop 5 path') of
            Left _ -> Left "Invalid absolute file path"
            Right path -> Right $ Logging'File path
    _ -> Left "Invalid logging setting"

readSwitchActionM :: ReadM SwitchAction
readSwitchActionM =
  eitherReader \case
    "check" -> Right SwitchAction'Check
    "switch" -> Right SwitchAction'Switch
    "boot" -> Right SwitchAction'Boot
    "test" -> Right SwitchAction'Test
    "dry-activate" -> Right SwitchAction'DryActivate
    _ -> Left "Invalid switch action"

parseDeploy :: Parser Command
parseDeploy =
  Deploy
    <$> (many (strArgument (metavar "HOST")) <&> nonEmpty)
    <*> option str (metavar "FLAKE" <> short 'f' <> long "flake")
    <*> switch (long "sudo" <> short 'S')
    <*> option readSwitchActionM (metavar "SWITCH-ACTION" <> short 'a' <> long "action")

parseFar :: Parser Command
parseFar = pure Far

parseReplay :: Parser Command
parseReplay =
  Replay
    <$> optional (option pathAbsFile (metavar "NIX_INTERNAL_LOG" <> long "nix-internal-log"))
    <*> optional (option auto (metavar "RATE" <> long "rate"))

parseCommand :: Parser Command
parseCommand =
  subparser
    (command "deploy" (info parseDeploy (progDesc "Deploy to hosts")))
    <|> subparser (command "far" (info parseFar (progDesc "Serve the far end of a SSH connection")) <> internal)
    <|> subparser (command "replay" (info parseReplay (progDesc "Replay a Nix internal log file")) <> internal)

parseLogging :: Parser Logging
parseLogging = option readLoggingM (metavar "LOGGING" <> short 'l' <> long "logging") <|> pure Logging'Blackhole

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseCommand
    <*> parseLogging
    <*> switch (long "nom" <> short 'n')
    <*> optional (option pathAbsFile (metavar "FILE" <> long "dump-log"))

options :: ParserInfo Options
options =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc "Mrsk a NixOS configurations to hosts"
        <> header "mrsk - yet another NixOS deployment tool"
    )
