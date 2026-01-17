{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Cli where

import Brick (resizeOrQuit)
import Brick.AttrMap
import Brick.BChan (newBChan, writeBChan)
import Brick.BChan qualified
import Brick.Main (App (..), customMain, halt, neverShowCursor)
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Util (on)
import Brick.Widgets.Core
import Control.Monad (forM, forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (fromJSON)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (singleton)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple.Extra (uncurry3)
import Effectful (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  Limit (..),
  Persistence (..),
  UnliftStrategy (..),
  withEffToIO,
  (:>),
 )
import Effectful.Concurrent (Concurrent, ThreadId, forkIO, myThreadId, throwTo)
import Effectful.Concurrent.Async (AsyncCancelled (..), withAsync)
import Effectful.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Effectful.Concurrent.MVar.Strict (MVar', newEmptyMVar', putMVar', takeMVar')
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem.IO (FileSystem, hFlush)
import Effectful.Monad.Logger (Logger, logDebugN)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Attributes qualified as V
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Config qualified
import Graphics.Vty.CrossPlatform qualified
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform (Lens', at, makeLensesWith, mapped, use, (%=), (.=), (^.), _3)
import NixMessage (NixJSONMessage, Verbosity (Info), maybeLevel, _level)
import NixMessage.Parser
import RequireCallStack (RequireCallStack)
import System.IO qualified as IO
import System.NixNG.TH (duplicateRules)

data CliEffect :: Effect where
  TellBuildStarted :: {attr :: Text} -> CliEffect m ()
  TellBuildFinished :: {attr :: Text} -> CliEffect m ()
  TellFoundConfigurations :: {configurations :: [Text]} -> CliEffect m ()
  TellNixInternalLog :: {internalLog :: LazyByteString} -> CliEffect m ()
  AskReadLine :: {prompt :: Text} -> CliEffect m Text
type instance DispatchOf CliEffect = Dynamic

data CliMessage
  = CliMessage'BuildStarted {attr :: Text}
  | CliMessage'BuildFinished {attr :: Text}
  | CliMessage'FoundConfigurations {configurations :: [Text]}
  | CliMessage'NixInternalLog {internalLog :: LazyByteString}
  | CliMessage'Readline {prompt :: Text, response :: MVar' Text}

data CliHandle = CliHandle {chan :: Chan CliMessage, threadId :: ThreadId}

data BuildState
  = BuildState'None
  | BuildState'Running
  | BuildState'Finished
  deriving (Show)

data CliState = CliState
  { knownBuilds :: HashMap Text BuildState
  , askpass :: Maybe (Text, MVar' Text, Text)
  }
makeLensesWith duplicateRules ''CliState

initialCliState :: CliState
initialCliState =
  CliState
    { knownBuilds = HM.empty
    , askpass = Nothing
    }

tellBuildStarted :: (CliEffect :> es, HasCallStack) => Text -> Eff es ()
tellBuildStarted attr = send (TellBuildStarted{attr})

tellBuildFinished :: (CliEffect :> es, HasCallStack) => Text -> Eff es ()
tellBuildFinished attr = send (TellBuildFinished{attr})

tellFoundConfigurations :: (CliEffect :> es, HasCallStack) => [Text] -> Eff es ()
tellFoundConfigurations configurations = send (TellFoundConfigurations{configurations})

tellNixInternalLog :: (CliEffect :> es, HasCallStack) => LazyByteString -> Eff es ()
tellNixInternalLog internalLog = send (TellNixInternalLog{internalLog})

askReadLine :: (CliEffect :> es, HasCallStack) => Text -> Eff es Text
askReadLine prompt = send (AskReadLine{prompt})

runCliEffect
  :: (Concurrent :> es, FileSystem :> es, HasCallStack, IOE :> es, Logger :> es, RequireCallStack)
  => Eff (CliEffect : es) a
  -> Eff es a
runCliEffect eff = do
  chan <- newChan
  mainThread <- myThreadId
  withAsync (mainLoop chan >> throwTo mainThread AsyncCancelled) \_threadId ->
    eff & interpret \_ -> \case
      TellBuildStarted{attr} -> writeChan chan CliMessage'BuildStarted{attr}
      TellBuildFinished{attr} -> writeChan chan CliMessage'BuildFinished{attr}
      TellFoundConfigurations{configurations} -> writeChan chan CliMessage'FoundConfigurations{configurations}
      TellNixInternalLog{internalLog} -> writeChan chan CliMessage'NixInternalLog{internalLog}
      AskReadLine{prompt} -> do
        response <- newEmptyMVar'

        writeChan chan CliMessage'Readline{prompt, response}

        takeMVar' response

myEvent
  :: (Concurrent :> es, Logger :> es, RequireCallStack)
  => (forall a. Eff es a -> IO a) -> BrickEvent n CliMessage -> EventM n CliState ()
myEvent unlift (AppEvent CliMessage'NixInternalLog{internalLog}) = liftIO . unlift $ logDebugN (T.show internalLog)
myEvent _ (AppEvent (CliMessage'BuildStarted name)) = _knownBuilds . at name .= Just BuildState'Running
myEvent _ (AppEvent (CliMessage'BuildFinished name)) = _knownBuilds . at name .= Just BuildState'Finished
myEvent _ (AppEvent (CliMessage'FoundConfigurations configurations)) = forM_ configurations \configuration ->
  _knownBuilds . at configuration .= Just BuildState'None
myEvent _ (AppEvent (CliMessage'Readline{prompt, response})) = _askpass .= Just (prompt, response, "")
myEvent _ (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt
myEvent _ (VtyEvent (EvKey KBS [])) = do
  askpass <- use _askpass

  case askpass of
    Just (_, _, acc) -> _askpass . mapped . _3 .= T.take (T.length acc - 1) acc
    Nothing -> pure ()
myEvent unlift (VtyEvent (EvKey KEnter [])) = do
  askpass <- use _askpass

  case askpass of
    Just (_, response, acc) -> do
      _askpass .= Nothing
      liftIO . unlift $ putMVar' response acc
    Nothing -> pure ()
myEvent _ (VtyEvent (EvKey (KChar c) [])) = do
  askpass <- use _askpass

  case askpass of
    Just (_, _, acc) -> _askpass . mapped . _3 .= acc `T.snoc` c
    Nothing -> pure ()
myEvent _ _ = pure ()

headingAttr :: AttrName
headingAttr = attrName "heading"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (headingAttr, V.defAttr `V.withStyle` V.bold)
    ]

makePrompt :: Text -> MVar' Text -> Text -> Widget n
makePrompt prompt _ acc = txt $ prompt <> acc

mainLoop
  :: (Concurrent :> es, FileSystem :> es, HasCallStack, IOE :> es, Logger :> es, RequireCallStack)
  => Chan CliMessage -> Eff es ()
mainLoop chan =
  withEffToIO SeqUnlift \unlift -> do
    let
      app :: App CliState CliMessage ()
      app =
        App
          { appDraw = \CliState{knownBuilds, askpass} ->
              [ vBox $
                  [ withAttr headingAttr $ str "machines"
                  ]
                    ++ ( HM.toList knownBuilds & map \(name, state) ->
                           hBox [txt name, txt "\t", txt (T.show state)]
                       )
                    ++ maybe [] (singleton . padTop Max . uncurry3 makePrompt) askpass
              ]
          , appHandleEvent = myEvent unlift
          , appStartEvent = return ()
          , appAttrMap = const theMap
          , appChooseCursor = neverShowCursor
          }

    eventChan <- liftIO $ newBChan 10
    unlift $ withAsync (forever $ readChan chan >>= liftIO . writeBChan eventChan) \_ -> do
      let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
      initialVty <- liftIO $ buildVty
      finalState <- liftIO $ customMain initialVty buildVty (Just eventChan) app initialCliState
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
