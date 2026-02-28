{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import ByteString.Aeson.Orphans ()
import Control.Monad.Logger (
  Loc (Loc),
  LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn),
  LogSource,
  LogStr,
  fromLogStr,
  loc_end,
  loc_filename,
  loc_module,
  loc_package,
  loc_start,
  toLogStr,
 )
import Data.Aeson.Types (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

instance FromJSON ExitCode where
  parseJSON = A.withScientific "ExitCode" \scientific ->
    pure $ case scientific of
      0 -> ExitSuccess
      _ -> ExitFailure $ floor scientific

instance ToJSON ExitCode where
  toJSON ExitSuccess = toJSON (0 :: Int)
  toJSON (ExitFailure exitCode) = toJSON exitCode

instance FromJSON Loc where
  parseJSON = A.withObject "Loc" \obj ->
    Loc
      <$> obj .: "filename"
      <*> obj .: "package"
      <*> obj .: "module"
      <*> obj .: "start"
      <*> obj .: "end"

instance ToJSON Loc where
  toJSON loc =
    A.object
      [ "filename" .= loc_filename loc
      , "package" .= loc_package loc
      , "module" .= loc_module loc
      , "start" .= loc_start loc
      , "end" .= loc_end loc
      ]

instance FromJSON LogLevel where
  parseJSON = A.withText "LogSource" \case
    "Debug" -> pure LevelDebug
    "Info" -> pure LevelInfo
    "Warn" -> pure LevelWarn
    "Error" -> pure LevelError
    other -> pure (LevelOther other)

instance ToJSON LogLevel where
  toJSON LevelDebug = A.String "Debug"
  toJSON LevelInfo = A.String "Info"
  toJSON LevelWarn = A.String "Warn"
  toJSON LevelError = A.String "Error"
  toJSON (LevelOther other) = A.String other

instance FromJSON LogStr where
  parseJSON value = parseJSON @ByteString value <&> toLogStr

instance ToJSON LogStr where
  toJSON logStr = toJSON $ fromLogStr logStr
