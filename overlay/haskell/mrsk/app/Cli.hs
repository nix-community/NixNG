{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Cli where

import Brick qualified as B
import Brick.AttrMap
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App (..), customMain, halt, neverShowCursor)
import Brick.Types (BrickEvent (..), EventM, Size (..), Widget (..), nestEventM, nestEventM')
import Brick.Widgets.Border (border, hBorder, hBorderWithLabel)
import Brick.Widgets.Core
import Brick.Widgets.Table qualified as Brick
import Cli.EscapeCode (escapeCodeToBrick, parseSomething)
import Cli.Machine (
  MachineState (..),
  Machines,
  emptyMachines,
  machineExpand,
  machinesNext,
  machinesPrev,
  renderMachine,
  renderMachines,
  _state,
 )
import Cli.Prompt (
  Prompt,
  PromptItem (PromptItem'Freeform, confidential, prompt, response'text),
  emptyPrompt,
  eventPrompt,
  promptActive,
  promptAddItem,
  renderPrompt,
 )
import Cli.ScrollableList (ScrollableList)
import Cli.ScrollableList qualified as ScrollableList
import Common (withChildReader)
import Control.Exception (ExceptionWithContext)
import Control.Exception.Base (SomeException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (singleton)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Debug.Trace (trace)
import Effectful (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  UnliftStrategy (..),
  withEffToIO,
  (:>),
 )
import Effectful.Concurrent (Concurrent, ThreadId, myThreadId, throwTo)
import Effectful.Concurrent.Async (AsyncCancelled (..), link, withAsync)
import Effectful.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Effectful.Concurrent.MVar.Strict (MVar', newEmptyMVar', putMVar', takeMVar')
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem.IO (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as BS
import Effectful.Monad.Logger (Logger, logDebugN, logErrorN)
import Effectful.Process.Typed (ExitCode (..), TypedProcess)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Attributes qualified as V
import Graphics.Vty.Attributes qualified as Vty
import Graphics.Vty.Attributes.Color qualified as Vty
import Graphics.Vty.Config qualified
import Graphics.Vty.CrossPlatform qualified
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform (at, makeLensesWith, mapped, use, view, (%=), (%~), (.=), (^.))
import NixMessage (MessageAction (..), NixJSONMessage (..))
import NixMessage.Parser
import RequireCallStack (RequireCallStack)
import System.NixNG.TH (duplicateRules)

data CliEffect :: Effect where
  TellBuildStarted :: {machine :: Text} -> CliEffect m ()
  TellEvaluationStarted :: {machine :: Text} -> CliEffect m ()
  TellCopyStarted :: {machine :: Text} -> CliEffect m ()
  TellDeploymentStarted :: {machine :: Text} -> CliEffect m ()
  TellDeploymentFinished :: {machine :: Text, exitCode :: ExitCode, output :: Text} -> CliEffect m ()
  TellException :: {machine :: Text, exception :: (ExceptionWithContext SomeException)} -> CliEffect m ()
  TellNixInternalLog :: {internalLog :: ByteString} -> CliEffect m ()
  AskReadLine :: {prompt :: Text} -> CliEffect m Text
  AskConfirmExit :: {mException :: Maybe SomeException} -> CliEffect m ()
type instance DispatchOf CliEffect = Dynamic

data CliMessage
  = CliMessage'BuildStarted {machine :: Text}
  | CliMessage'CopyStarted {machine :: Text}
  | CliMessage'EvaluationStarted {machine :: Text}
  | CliMessage'DeploymentStarted {machine :: Text}
  | CliMessage'DeploymentFinished {machine :: Text, exitCode :: ExitCode, output :: Text}
  | CliMessage'Exception {machine :: Text, exception :: ExceptionWithContext SomeException}
  | CliMessage'NixInternalLog {internalLog :: ByteString}
  | CliMessage'Readline {prompt :: Text, response :: MVar' Text}
  | CliMessage'ConfirmExit {mException :: Maybe SomeException, confirmed :: MVar' ()}

data CliHandle = CliHandle {chan :: Chan CliMessage, threadId :: ThreadId}

data Cli'AskPass = Cli'AskPass
  { prompt :: Text
  , response :: MVar' Text
  , accumulator :: Text
  }

data Cli'ConfirmExit = Cli'ConfirmExit
  { confirmed :: MVar' ()
  , mException :: Maybe SomeException
  }

data CliState = CliState
  { machines :: Machines Name
  , prompt :: Prompt
  , confirmExit :: Maybe Cli'ConfirmExit
  , nixMessages :: ScrollableList (Widget Name)
  }

data Name = LogScroll
  deriving (Eq, Ord)

data Config
  = Config
  { nom :: Bool
  }

makeLensesWith duplicateRules ''Config
makeLensesWith duplicateRules ''CliState
makeLensesWith duplicateRules ''Cli'AskPass
makeLensesWith duplicateRules ''Cli'ConfirmExit

initialCliState :: CliState
initialCliState =
  CliState
    { machines = emptyMachines
    , prompt = emptyPrompt
    , confirmExit = Nothing
    , nixMessages = ScrollableList.empty
    }

tellBuildStarted :: (CliEffect :> es, HasCallStack) => Text -> Eff es ()
tellBuildStarted machine = send (TellBuildStarted{machine})

tellEvaluationStarted :: (CliEffect :> es, HasCallStack) => Text -> Eff es ()
tellEvaluationStarted machine = send (TellEvaluationStarted{machine})

tellCopyStarted :: (CliEffect :> es, HasCallStack) => Text -> Eff es ()
tellCopyStarted machine = send (TellCopyStarted{machine})

tellDeploymentStarted :: (CliEffect :> es, HasCallStack) => Text -> Eff es ()
tellDeploymentStarted machine = send (TellDeploymentStarted{machine})

tellDeploymentFinished :: (CliEffect :> es, HasCallStack) => Text -> ExitCode -> Text -> Eff es ()
tellDeploymentFinished machine exitCode output = send (TellDeploymentFinished{machine, exitCode, output})

tellException :: (CliEffect :> es, HasCallStack) => Text -> ExceptionWithContext SomeException -> Eff es ()
tellException machine exception = send (TellException{machine, exception})

tellNixInternalLog :: (CliEffect :> es, HasCallStack) => ByteString -> Eff es ()
tellNixInternalLog internalLog = send (TellNixInternalLog{internalLog})

askReadLine :: (CliEffect :> es, HasCallStack) => Text -> Eff es Text
askReadLine prompt = send (AskReadLine{prompt})

askConfirmExit :: (CliEffect :> es, HasCallStack) => Maybe SomeException -> Eff es ()
askConfirmExit exception = send (AskConfirmExit exception)

withNom :: (FileSystem :> es, IOE :> es, TypedProcess :> es) => ((ByteString -> Eff es ()) -> Eff es a) -> Eff es a
withNom act = withChildReader "alacritty" "nom --json" \hIn -> act (BS.hPutStrLn hIn)

runCliEffect
  :: (Concurrent :> es, FileSystem :> es, HasCallStack, IOE :> es, Logger :> es, RequireCallStack, TypedProcess :> es)
  => Config
  -> Eff (CliEffect : es) a
  -> Eff es a
runCliEffect Config{nom} eff = do
  let maybeWithNom =
        if nom
          then withNom
          else \act -> act (const $ pure ())

  chan <- newChan
  mainThread <- myThreadId
  withAsync (mainLoop chan >> throwTo mainThread AsyncCancelled) \a -> do
    link a
    maybeWithNom \sendToNom ->
      eff & interpret \_ -> \case
        TellBuildStarted{machine} -> writeChan chan CliMessage'BuildStarted{machine}
        TellEvaluationStarted{machine} -> writeChan chan CliMessage'EvaluationStarted{machine}
        TellCopyStarted{machine} -> writeChan chan CliMessage'CopyStarted{machine}
        TellDeploymentStarted{machine} -> writeChan chan CliMessage'DeploymentStarted{machine}
        TellDeploymentFinished{machine, exitCode, output} -> writeChan chan CliMessage'DeploymentFinished{machine, exitCode, output}
        TellException{machine, exception} -> writeChan chan CliMessage'Exception{machine, exception}
        TellNixInternalLog{internalLog} -> do
          sendToNom internalLog
          writeChan chan CliMessage'NixInternalLog{internalLog}
        AskReadLine{prompt} -> do
          response <- newEmptyMVar'

          writeChan chan CliMessage'Readline{prompt, response}

          takeMVar' response
        AskConfirmExit{mException} -> do
          confirmed <- newEmptyMVar'

          writeChan chan CliMessage'ConfirmExit{mException, confirmed}

          takeMVar' confirmed

eventConfirmExit
  :: (Concurrent :> es, Logger :> es, RequireCallStack)
  => (forall a. Eff es a -> IO a) -> BrickEvent n CliMessage -> EventM n Cli'ConfirmExit Bool
eventConfirmExit unlift (VtyEvent (EvKey KEnter [])) = do
  confirm <- use _confirmed

  liftIO . unlift $ putMVar' confirm ()
  pure False
eventConfirmExit _ _ = pure True

myEvent
  :: (Concurrent :> es, Logger :> es, RequireCallStack)
  => (forall a. Eff es a -> IO a) -> BrickEvent Name CliMessage -> EventM Name CliState ()
myEvent unlift (AppEvent CliMessage'NixInternalLog{internalLog = internalLog'}) = case parseJSONLine internalLog' of
  Left err -> liftIO . unlift . logErrorN $ "Could not deserialize: " <> T.show (T.decodeUtf8' internalLog') <> T.pack err
  Right (Message MkMessageAction{message = message'}) -> do
    case Atto.parseOnly parseSomething message' of
      Right message ->
        _nixMessages %= \i ->
          ScrollableList.addItem
            ( hBox
                (map (\(attr, text) -> modifyDefAttr (const attr) (txt text)) $ escapeCodeToBrick message defAttr)
            )
            i
      Left err -> liftIO . unlift $ logDebugN ("could not parse Nix message" <> T.show err)
  Right _internalLog -> pure ()
-- liftIO . unlift . logDebugN $ T.show internalLog
myEvent _ (AppEvent CliMessage'BuildStarted{machine}) = _machines . at machine .= Just MachineState'Building
myEvent _ (AppEvent CliMessage'CopyStarted{machine}) = _machines . at machine .= Just MachineState'Copying
myEvent _ (AppEvent CliMessage'EvaluationStarted{machine}) = _machines . at machine .= Just MachineState'Evaluating
myEvent _ (AppEvent CliMessage'DeploymentStarted{machine}) = _machines . at machine .= Just MachineState'Deploying
myEvent _ (AppEvent CliMessage'DeploymentFinished{machine, exitCode, output}) = _machines . at machine .= Just MachineState'Deployed{exitCode, output}
myEvent _ (AppEvent CliMessage'Exception{machine, exception}) = _machines . at machine .= Just MachineState'Exception{exception}
myEvent unlift (AppEvent CliMessage'Readline{prompt, response}) = do
  liftIO . unlift . logDebugN $ "asking user for input with prompt\"" <> prompt <> "\""
  _prompt %= (`promptAddItem` PromptItem'Freeform{prompt, response'text = response, confidential = True})
myEvent _ (AppEvent CliMessage'ConfirmExit{mException, confirmed}) = _confirmExit .= Just Cli'ConfirmExit{mException, confirmed}
myEvent _ (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt
myEvent unlift event = do
  prompt <- use _prompt
  if promptActive prompt
    then do nestEventM' prompt (eventPrompt unlift event) >>= (_prompt .=)
    else case event of
      (VtyEvent (EvKey (KChar 'l') [])) -> _machines %= machinesPrev
      (VtyEvent (EvKey (KChar 'n') [])) -> _machines %= machinesNext
      (VtyEvent (EvKey (KChar 'ß') [])) -> _machines %= machineExpand
      _ -> pure ()

  use _confirmExit >>= \case
    Just confirmExit -> do
      (s, a) <- nestEventM confirmExit $ eventConfirmExit unlift event

      if a
        then _confirmExit . mapped .= s
        else _confirmExit .= Nothing
    Nothing -> pure ()

  use _nixMessages >>= (`nestEventM'` (ScrollableList.handleEvent LogScroll unlift event)) >>= (_nixMessages .=)

  pure ()

headingAttr :: AttrName
headingAttr = attrName "heading"

altListAttr :: AttrName
altListAttr = attrName "alt_list"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (headingAttr, V.defAttr `V.withStyle` V.bold)
    , (altListAttr, V.defAttr `Vty.withBackColor` (Vty.Color240 0))
    ]

mainLoop
  :: (Concurrent :> es, FileSystem :> es, HasCallStack, IOE :> es, Logger :> es, RequireCallStack)
  => Chan CliMessage -> Eff es ()
mainLoop chan =
  withEffToIO SeqUnlift \unlift -> do
    let
      app :: App CliState CliMessage Name
      app =
        App
          { appDraw = \CliState{machines, prompt, confirmExit, nixMessages} ->
              [ vBox $
                  [ renderMachines altListAttr machines
                  , hBorder
                  , vLimit 15 $ (\how -> padRight Max $ ScrollableList.render LogScroll how nixMessages) id
                  , renderPrompt prompt
                  ]
                    ++ maybe
                      []
                      ( \c -> case c ^. _mException of
                          Just exception -> [padTop Max $ txt ("confirm exit: " <> T.show exception)]
                          Nothing -> [padTop Max $ txt "confirm exit"]
                      )
                      confirmExit
              ]
          , appHandleEvent = myEvent unlift
          , appStartEvent = return ()
          , appAttrMap = const theMap
          , appChooseCursor = neverShowCursor
          }

    eventChan <- liftIO $ newBChan 10
    mainThread <- unlift $ myThreadId
    unlift $ withAsync (forever (readChan chan >>= liftIO . writeBChan eventChan) >> throwTo mainThread AsyncCancelled) \_ -> do
      let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
      initialVty <- liftIO $ buildVty
      _ <- liftIO $ customMain initialVty buildVty (Just eventChan) app initialCliState
      pure ()

-- Use finalState and exit

-- forever do
--   message <- readChan chan

--   case message of
--     CliMessage'BuildStarted{attr} ->
--       liftIO . T.putStrLn $ "build started " <> attr
--     CliMessage'BuildFinished{attr} ->
--       liftIO . T.putStrLn $ "build finished " <> attr
--     CliMessage'FoundConfigurations{configurations} ->
--       liftIO . T.putStrLn $ "[ " <> T.intercalate " " configurations <> " ]"
--     CliMessage'NixInternalLog{internalLog} -> do
--       case parseJSONLine $ internalLog of
--         Right nixMessage ->
--           if maybe False id $ maybeLevel nixMessage <&> (<= Info)
--             then
--               liftIO . T.putStrLn . T.show $ nixMessage
--             else pure ()
--         Left err ->
--           liftIO $ T.putStrLn ("failed to decode with " <> T.pack err)
--     CliMessage'Readline{prompt, response} -> do
--       liftIO (T.putStr prompt) *> hFlush IO.stdout >> liftIO getLine >>= (response `putMVar'`) . T.pack
