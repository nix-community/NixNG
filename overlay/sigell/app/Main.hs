-- SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
--
-- SPDX-License-Identifier: MPL-2.0

module Main where
import System.Posix.Signals
import Control.Concurrent.Chan
import Foreign.C (CInt)
import Control.Concurrent (putMVar)

import Data.Aeson (encode, eitherDecode);
import Data.ByteString.Lazy.Char8 (pack);

import Config
import CmdLine

import Data.Functor ((<&>))
import System.Posix.Process
import System.Posix.Types
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Environment (setEnv)
import System.Console.CmdArgs (cmdArgs)

catchSignals :: [Signal] -> IO [Signal]
catchSignals sigs = do
  chan <- newChan
  mapM_ (\sig -> installHandler sig (Catch $ writeSignal' sig chan) Nothing) sigs
  getChanContents chan
    where
      writeSignal' sig chan = do
        writeChan chan sig

catchSignal :: ProcessID -> Entry -> IO ()
catchSignal child entry = do
  installHandler sig (Catch action) Nothing
  return ()
  where sig = entrySignal entry
        action = case entryAction entry of
          Exec command env -> actionExec command env
          Signal rewrite selector -> case selector of
            Pid p -> signalProcess (fromMaybe sig rewrite) (CPid (fromIntegral p))
            Child -> signalProcess (fromMaybe sig rewrite) child

        actionExec c env = void $ forkProcess
          (void $ do
            mapM_ (uncurry setEnv) (fromMaybe [] env)
            executeFile (command c) True (args c) Nothing)
        command = head
        args = tail

readEOF :: String -> String
readEOF s = unlines $ readEOF' $ lines s
  where readEOF' ("EOF":xs) = []
        readEOF' (x:xs) = x : readEOF' xs

readConfig :: String -> Either String Config
readConfig = eitherDecode . pack

forkedProg :: Config -> IO ()
forkedProg c = do
  getProcessID >>= createProcessGroupFor
  mapM_ (uncurry setEnv) (fromMaybe [] env)
  executeFile command True args Nothing
  where command = head $ configCommand c
        args = tail $ configCommand c
        env = configEnvironment c

waitForProcess :: CPid -> IO ()
waitForProcess p = void $ getProcessStatus True False p

main :: IO ()
main = do
  -- sigs <- catchSignals [ keyboardSignal, sigHUP, sigUSR1 ]
  -- mapM_ print sigs
  -- str <- getLine :: IO String
  -- putStrLn str
  -- print $ encode ([ 1, 2, 3 ] :: [Integer])
  opts <- cmdArgs cmdline
  mc <- readFile (cmdLineConfigFile opts) <&> readConfig
  case mc of
    Right c -> do
      process <- forkProcess $ forkedProg c

      mapM_ (catchSignal process) $ configEntries c

      waitForProcess process
      return ()
    Left s -> do
      putStrLn ("Invalid Config: " ++ s)
  putStrLn "Bye..."
  return ()

