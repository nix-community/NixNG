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
import Brick.Types (BrickEvent (..), EventM, Widget, modify, nestEventM, nestEventM')
import Brick.Util (on)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core
import Cli.ScrollableList (ScrollableList)
import Cli.ScrollableList qualified as ScrollableList
import Common (withChildReader)
import Control.Exception.Base (SomeException)
import Control.Monad (forM, forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (fromJSON)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind (Type)
import Data.List (singleton)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Tuple.Extra (uncurry3)
import Effectful (
  Dispatch (Dynamic, Static),
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
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep)
import Effectful.FileSystem.IO (FileSystem, hFlush)
import Effectful.Monad.Logger (Logger, logDebugN, logErrorN)
import Effectful.Process.Typed (TypedProcess)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Attributes qualified as V
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Config qualified
import Graphics.Vty.CrossPlatform qualified
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform (Lens', at, makeLensesWith, mapped, use, (%=), (.=), (^.), _3)
import NixMessage (MessageAction (..), NixJSONMessage (..), Verbosity (Info), maybeLevel, _level)
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
  AskConfirmExit :: {exception :: Maybe SomeException} -> CliEffect m ()
type instance DispatchOf CliEffect = Dynamic

data CliMessage
  = CliMessage'BuildStarted {attr :: Text}
  | CliMessage'BuildFinished {attr :: Text}
  | CliMessage'FoundConfigurations {configurations :: [Text]}
  | CliMessage'NixInternalLog {internalLog :: LazyByteString}
  | CliMessage'Readline {prompt :: Text, response :: MVar' Text}
  | CliMessage'ConfirmExit {exception :: Maybe SomeException, confirmed :: MVar' ()}

data CliHandle = CliHandle {chan :: Chan CliMessage, threadId :: ThreadId}

data BuildState
  = BuildState'None
  | BuildState'Running
  | BuildState'Finished
  deriving (Show)

data Cli'AskPass = Cli'AskPass
  { prompt :: Text
  , response :: MVar' Text
  , accumulator :: Text
  }

data Cli'ConfirmExit = Cli'ConfirmExit
  { confirmed :: MVar' ()
  , exception :: Maybe SomeException
  }

data CliState = CliState
  { knownBuilds :: HashMap Text BuildState
  , askpass :: [Cli'AskPass]
  , confirmExit :: Maybe Cli'ConfirmExit
  , nixMessages :: ScrollableList Text
  }

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
    { knownBuilds = HM.empty
    , askpass = []
    , confirmExit = Nothing
    , nixMessages = ScrollableList.empty
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

askConfirmExit :: (CliEffect :> es, HasCallStack) => Maybe SomeException -> Eff es ()
askConfirmExit exception = send (AskConfirmExit exception)

withNom :: (FileSystem :> es, IOE :> es, TypedProcess :> es) => ((LazyByteString -> Eff es ()) -> Eff es a) -> Eff es a
withNom act = withChildReader "alacritty" "nom --json" \hIn -> act (liftIO . BSL.hPutStrLn hIn)

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
  withAsync (mainLoop chan >> throwTo mainThread AsyncCancelled) \_threadId ->
    maybeWithNom \sendToNom ->
      eff & interpret \_ -> \case
        TellBuildStarted{attr} -> writeChan chan CliMessage'BuildStarted{attr}
        TellBuildFinished{attr} -> writeChan chan CliMessage'BuildFinished{attr}
        TellFoundConfigurations{configurations} -> writeChan chan CliMessage'FoundConfigurations{configurations}
        TellNixInternalLog{internalLog} -> do
          sendToNom internalLog
          writeChan chan CliMessage'NixInternalLog{internalLog}
        AskReadLine{prompt} -> do
          response <- newEmptyMVar'

          writeChan chan CliMessage'Readline{prompt, response}

          takeMVar' response
        AskConfirmExit{exception} -> do
          confirmed <- newEmptyMVar'

          writeChan chan CliMessage'ConfirmExit{exception, confirmed}

          takeMVar' confirmed

eventConfirmExit
  :: (Concurrent :> es, Logger :> es, RequireCallStack)
  => (forall a. Eff es a -> IO a) -> BrickEvent n CliMessage -> EventM n Cli'ConfirmExit Bool
eventConfirmExit unlift (VtyEvent (EvKey KEnter [])) = do
  confirm <- use _confirmed

  liftIO . unlift $ putMVar' confirm ()
  pure False
eventConfirmExit _ _ = pure True

eventAskSudoPass
  :: (Concurrent :> es, Logger :> es, RequireCallStack)
  => (forall a. Eff es a -> IO a) -> BrickEvent n CliMessage -> EventM n Cli'AskPass Bool
eventAskSudoPass _ (VtyEvent (EvKey KBS [])) = pure True <* (_accumulator %= \acc -> T.take (T.length acc - 1) acc)
eventAskSudoPass unlift (VtyEvent (EvKey KEnter [])) = do
  response <- use _response
  accumulator <- use _accumulator

  liftIO . unlift $ putMVar' response accumulator

  pure False
eventAskSudoPass _ (VtyEvent (EvKey (KChar c) [])) = pure True <* (_accumulator %= (`T.snoc` c))
eventAskSudoPass _ _ = pure True

myEvent
  :: (Concurrent :> es, Logger :> es, RequireCallStack)
  => (forall a. Eff es a -> IO a) -> BrickEvent Name CliMessage -> EventM Name CliState ()
myEvent unlift (AppEvent CliMessage'NixInternalLog{internalLog = internalLog'}) = case parseJSONLine internalLog' of
  Left err -> liftIO . unlift . logErrorN $ "Could not deserialize: " <> T.show (TL.decodeUtf8' internalLog') <> T.pack err
  Right (Message MkMessageAction{level, message}) ->
    _nixMessages %= \i -> foldr ScrollableList.addItem i (map ((("[" <> T.show level <> "] ") <>)) $ T.lines message)
  Right _internalLog -> pure ()
-- liftIO . unlift . logDebugN $ T.show internalLog
myEvent _ (AppEvent CliMessage'BuildStarted{attr}) = _knownBuilds . at attr .= Just BuildState'Running
myEvent _ (AppEvent CliMessage'BuildFinished{attr}) = _knownBuilds . at attr .= Just BuildState'Finished
myEvent _ (AppEvent CliMessage'FoundConfigurations{configurations}) = forM_ configurations \configuration ->
  _knownBuilds . at configuration .= Just BuildState'None
myEvent _ (AppEvent CliMessage'Readline{prompt, response}) = _askpass %= (++ [Cli'AskPass{prompt, response, accumulator = ""}])
myEvent _ (AppEvent CliMessage'ConfirmExit{exception, confirmed}) = _confirmExit .= Just Cli'ConfirmExit{exception, confirmed}
myEvent _ (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt
myEvent unlift event = do
  use _askpass >>= \case
    askpass : rest -> do
      (s, a) <- nestEventM askpass $ eventAskSudoPass unlift event

      if a
        then _askpass .= s : rest
        else _askpass .= rest
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

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (headingAttr, V.defAttr `V.withStyle` V.bold)
    ]

makePrompt :: Cli'AskPass -> Widget n
makePrompt Cli'AskPass{prompt, accumulator} = txt $ prompt <> (T.replicate (T.length accumulator) "*")

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay _ = Nothing

data Name = LogScroll
  deriving (Eq, Ord)

mainLoop
  :: (Concurrent :> es, FileSystem :> es, HasCallStack, IOE :> es, Logger :> es, RequireCallStack)
  => Chan CliMessage -> Eff es ()
mainLoop chan =
  withEffToIO SeqUnlift \unlift -> do
    let
      app :: App CliState CliMessage Name
      app =
        App
          { appDraw = \CliState{knownBuilds, askpass, confirmExit, nixMessages} ->
              [ vBox $
                  [ withAttr headingAttr $ str "machines"
                  ]
                    ++ ( HM.toList knownBuilds & map \(name, state) ->
                           hBox [txt name, txt "\t", txt (T.show state)]
                       )
                    ++ [ border . vLimit 15 $ (\how -> ScrollableList.render LogScroll how nixMessages) txt
                       ]
                    ++ maybe [] (singleton . padTop Max . makePrompt) (headMay askpass)
                    ++ maybe
                      []
                      ( \c -> case c ^. _exception of
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
