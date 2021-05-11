module Main where
import System.Posix.Signals
import Control.Concurrent.Chan
import Foreign.C (CInt)
import Debug.Trace (trace)
import Control.Concurrent (putMVar)

import Data.Aeson (encode, eitherDecode);
import Data.ByteString.Lazy.Char8 as C8;

import Config
import Data.Functor ((<&>))
import System.Posix.Process
import System.Posix.Types
import Control.Monad
import Data.Maybe (fromMaybe)

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
          Exec command -> actionExec command
          Signal rewrite selector -> case selector of
            Pid p -> signalProcess (fromMaybe sig rewrite) (CPid (fromIntegral p))
            Child -> signalProcess (fromMaybe sig rewrite) (trace (show child) child)

        actionExec c = do
          forkProcess $ executeFile (command c) True (args c) Nothing
          return ()
        command = Prelude.head
        args = Prelude.tail

readEOF :: String -> String
readEOF s = Prelude.unlines $ readEOF' $ Prelude.lines s
  where readEOF' ("EOF":xs) = []
        readEOF' (x:xs) = x : readEOF' xs

readConfig :: String -> Either String Config
readConfig = eitherDecode . C8.pack

forkedProg :: Config -> IO ()
forkedProg c = do
  getProcessID >>= createProcessGroupFor
  executeFile command True args Nothing
  where command = Prelude.head $ configCommand c
        args = Prelude.tail $ configCommand c

waitForProcess :: CPid -> IO ()
waitForProcess p = void $ getProcessStatus True False p

main :: IO ()
main = do
  -- sigs <- catchSignals [ keyboardSignal, sigHUP, sigUSR1 ]
  -- mapM_ print sigs
  -- str <- getLine :: IO String
  -- putStrLn str
  -- print $ encode ([ 1, 2, 3 ] :: [Integer])
  mc <- Prelude.getContents <&> readEOF >>= pure . readConfig
  case mc of
    Right c -> do
      process <- forkProcess $ forkedProg c

      mapM_ (catchSignal process) $ configEntries c

      waitForProcess process
      return ()
    Left s -> do
      Prelude.putStrLn ("Invalid Config: " ++ s)
  Prelude.putStrLn "Bye..."
  return ()

