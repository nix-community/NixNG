{-# LANGUAGE LambdaCase #-}

module Options where

import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Path.Posix (Abs, Dir, File, Path, parseAbsFile)
import System.NixNG.SomePath (pathAbsDir)

data Command
  = Deploy {hosts :: Maybe (NonEmpty Text), flake :: Text, sudo :: Bool}
  | Far

data Logging
  = Logging'OtherTerminal
  | Logging'File (Path Abs File)
  | Logging'Blackhole
  | Logging'Stderr

data Options = Options
  { command :: Command
  , logging :: Logging
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

parseDeploy :: Parser Command
parseDeploy =
  Deploy
    <$> (many (strArgument (metavar "HOST")) <&> nonEmpty)
    <*> option str (metavar "FLAKE" <> short 'f' <> long "flake")
    <*> switch (long "sudo" <> short 'S')

parseFar :: Parser Command
parseFar = pure Far

parseCommand :: Parser Command
parseCommand =
  subparser
    (command "deploy" (info parseDeploy (progDesc "Deploy to hosts")))
    <|> subparser (command "far" (info parseFar (progDesc "Serve the far end of a SSH connection")) <> internal)

parseLogging :: Parser Logging
parseLogging = option readLoggingM (metavar "LOGGING" <> short 'l' <> long "logging")

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseCommand
    <*> parseLogging

options :: ParserInfo Options
options =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc "Mrsk a NixOS configurations to hosts"
        <> header "mrsk - yet another NixOS deployment tool"
    )
