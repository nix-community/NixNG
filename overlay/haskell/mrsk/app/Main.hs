{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Cli (CliEffect, runCliEffect, tellBuildFinished, tellBuildStarted, tellFoundConfigurations)
import Control.Exception (Exception, throw)
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Key qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.Text qualified as A
import Data.Aeson.Types qualified as A
import Data.Either (fromLeft, fromRight)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (Concurrently (..))
import Effectful.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Exception (bracket_, throwIO)
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO (stdout)
import Effectful.FileSystem.IO.ByteString (hGetLine, hPutStr)
import Effectful.Monad.Logger (
  Logger,
  logDebugN,
  logInfoN,
  runFileLogging,
  runOtherTerminalLogging,
  runStderrLogging,
  runVoidLogging,
 )
import Effectful.Prim (Prim, runPrim)
import Effectful.Process.Typed (TypedProcess, runTypedProcess)
import GHC.Generics (Generic)
import GHC.Stack.Types (HasCallStack)
import Nix (Nix, nixBuild, nixEval, nixSelect, runNixEffect)
import Nix.Select (NixTarget (NixDerivation, NixFlake), Selector (..), nixFlakeInfo, select)
import Options (Command (..), Logging (..), Options (Options, command, logging), options)
import Options.Applicative (execParser)
import Path.Posix (Abs, Dir, File, Path, absdir, parseAbsDir, parseAbsFile, toFilePath)
import Remote (
  Near (..),
  RemoteEffect,
  SwitchAction (SwitchAction'DryActivate, SwitchAction'Switch),
  near,
  openConnection,
  runRemote,
 )
import Remote.Far (serveFar)
import RequireCallStack (RequireCallStack, provideCallStack)

data NixOSConfiguration
  = NixOSConfiguration
  { targetHost :: Text
  , remoteUser :: Text
  }
  deriving stock (Generic)
  deriving anyclass (A.FromJSON)

data NixSelectException
  = NixSelectException Text
  deriving (Show)
instance Exception NixSelectException

getNixOSConfigurations :: (Nix :> es) => Maybe [Text] -> Text -> Eff es (HashMap Text NixOSConfiguration)
getNixOSConfigurations hosts flake =
  nixSelect
    [
      (
        [ SelectorStr "nixosConfigurations"
        , maybe SelectorAll (SelectorSet . map SelectorStr) hosts
        , SelectorStr "config"
        , SelectorStr "mrsk"
        , SelectorAll
        ]
      , \case
          Right a -> pure a
          Left e -> throwIO (NixSelectException e)
      )
    ]
    flake
    <&> head

traverseThrottled :: (Concurrent :> es, Traversable t) => Int -> (a -> Eff es b) -> t a -> Eff es (t b)
traverseThrottled concLevel action taskContainer = do
  sem <- newQSem concLevel
  let throttledAction = bracket_ (waitQSem sem) (signalQSem sem) . action
  runConcurrently (traverse (Concurrently . throttledAction) taskContainer)

type TopLevelEffects =
  [ RemoteEffect
  , Environment
  , Nix
  , CliEffect
  , Logger
  , TypedProcess
  , FileSystem
  , Concurrent
  , Fail
  , Prim
  , IOE
  ]

mrsk :: (RequireCallStack) => Options -> Eff TopLevelEffects ()
mrsk Options{command = Far} = serveFar
mrsk Options{command = Deploy{hosts, flake, sudo, action}} = do
  configurations <- case hosts of
    Just hosts' -> getNixOSConfigurations (Just (NE.toList hosts')) flake
    Nothing -> getNixOSConfigurations Nothing flake

  tellFoundConfigurations (HM.keys configurations)

  derivations :: HashMap Text (Path Abs File) <-
    HM.fromList
      <$> traverseThrottled
        4
        ( \name -> do
            let attr = "nixosConfigurations.\"" <> name <> "\".config.system.build.toplevel.drvPath"
            (nixEval @Text . NixFlake $ flake <> "#" <> attr)
              >>= parseAbsFile . T.unpack
              <&> (name,)
        )
        (HM.keys configurations)

  builtPaths :: HashMap Text (Path Abs Dir) <-
    HM.fromList
      <$> traverseThrottled
        4
        ( \(name, derivation) ->
            tellBuildStarted name
              >> ( nixBuild (NixDerivation derivation)
                     <&> (name,) . fromLeft undefined . head
                 )
                <* tellBuildFinished name
        )
        (HM.toList derivations)

  forM_ (HM.toList configurations) \(name, NixOSConfiguration{remoteUser, targetHost}) -> do
    handle <- openConnection remoteUser targetHost

    output <-
      near handle $
        Near'SwitchNixos
          { configuration = (builtPaths HM.! name)
          , action
          , sudo
          }

    pure ()
  -- liftIO . T.putStrLn $ T.show output

  pure ()

main :: IO ()
main =
  provideCallStack $
    liftIO (execParser options) >>= \opts@Options{logging} ->
      effectStack logging $ do
        mrsk opts
        case logging of
          Logging'OtherTerminal -> do
            logInfoN "Press any key to continue"
            void $ liftIO getLine
          _ -> pure ()
 where
  effectStack
    :: (RequireCallStack)
    => Logging
    -> Eff TopLevelEffects a
    -> IO a
  effectStack logging =
    runEff
      . runPrim
      . runFailIO
      . runConcurrent
      . runFileSystem
      . runTypedProcess
      . ( case logging of
            Logging'OtherTerminal -> runOtherTerminalLogging
            Logging'File path -> runFileLogging path
            Logging'Blackhole -> runVoidLogging
            Logging'Stderr -> runStderrLogging
        )
      . runCliEffect
      . runNixEffect
      . runEnvironment
      . runRemote
