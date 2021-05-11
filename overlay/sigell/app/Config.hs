{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Config where

import System.Posix.Signals (Signal, sigABRT, sigALRM, sigBUS, sigCHLD,
                             sigCONT, sigFPE, sigHUP, sigILL, sigKILL,
                             sigQUIT, sigSEGV, sigSTOP, sigTTIN, sigTTOU,
                             sigUSR1, sigUSR2, sigPOLL, sigPROF, sigSYS,
                             sigTRAP, sigURG, sigVTALRM, sigXCPU, sigXFSZ, sigTERM, sigTSTP)
import Data.Aeson.Types ((.:))
import Data.Aeson as A
import Data.Aeson.Types as AT
import Data.Vector as V
import Data.HashMap.Strict as H
import GHC.Base ((<|>))

data SignalSelector = Pid Int | Child
  deriving (Show)

instance A.FromJSON SignalSelector where
  parseJSON (A.Object v)
    = v .: "type" >>= s 
    where s :: String -> AT.Parser SignalSelector
          s "pid" = Pid <$> v .: "pid"
          s "child" = pure Child
          s _ = fail "Invalid SignalSelector type"

data Action = Exec { actionExecCommand :: [String] }
            | Signal { actionSignalRewrite :: Maybe Signal,
                       actionSignalSelector :: SignalSelector }
  deriving (Show)

instance A.FromJSON Action where
  parseJSON (A.Object v)
    = v .: "type" >>= s
    where s :: String -> AT.Parser Action
          s "exec" = Exec <$> v .: "command"
          s "signal" = Signal <$> (v .: "rewrite" >>= parseSignalMaybe) <*> v .: "selector"

data Entry = Entry { entrySignal :: Signal,
                     entryAction :: Action }
  deriving (Show)

parseSignal :: String -> AT.Parser Signal
parseSignal "ABRT" = pure sigABRT
parseSignal "ALRM" = pure sigALRM
parseSignal "BUS" = pure sigBUS
parseSignal "CHLD" = pure sigCHLD
parseSignal "CONT" = pure sigCONT
parseSignal "FPE" = pure sigFPE
parseSignal "HUP" = pure sigHUP
parseSignal "ILL" = pure sigILL
parseSignal "KILL" = pure sigKILL
parseSignal "QUIT" = pure sigQUIT
parseSignal "SEGV" = pure sigSEGV
parseSignal "STOP" = pure sigSTOP
parseSignal "TERM" = pure sigTERM
parseSignal "TSTP" = pure sigTSTP 
parseSignal "TTIN" = pure sigTTIN
parseSignal "TTOU" = pure sigTTOU
parseSignal "USR1" = pure sigUSR1
parseSignal "USR2" = pure sigUSR2
parseSignal "POLL" = pure sigPOLL
parseSignal "PROF" = pure sigPROF
parseSignal "SYS" = pure sigSYS
parseSignal "TRAP" = pure sigTRAP
parseSignal "URG" = pure sigURG
parseSignal "VTALRM" = pure sigVTALRM 
parseSignal "XCPU" = pure sigXCPU 
parseSignal "XFSZ" = pure sigXFSZ 
parseSignal _ = fail "Invalid signal"

parseSignalMaybe :: Maybe String -> Parser (Maybe Signal)
parseSignalMaybe (Just s) = fmap Just (parseSignal s) 
parseSignalMaybe Nothing = pure Nothing 

instance A.FromJSON Entry where
  parseJSON (A.Object v)
    = Entry
    <$> (v .: "signal" >>= parseSignal)
    <*> v .: "action"

data Config = Config { configEntries :: [Entry],
                       configCommand :: [String] }
  deriving (Show)

instance A.FromJSON Config where
  parseJSON (A.Object v)
    = Config
    <$> a 
    <*> v .: "command"
    where a = entries >>= Prelude.mapM parseJSON
          entries = case v H.! "entries" of
                      A.Array v -> pure $ V.toList v
                      _ -> fail "Config filed `entries` not of type `Array`"
