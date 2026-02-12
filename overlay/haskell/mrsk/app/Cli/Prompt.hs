{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Cli.Prompt where

import Brick.Types (BrickEvent (VtyEvent), EventM, Widget)
import Brick.Widgets.Core (emptyWidget, txt, (<+>))
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, liftIO, (:>))
import Effectful.Concurrent.MVar.Strict (Concurrent, MVar', putMVar')
import Effectful.Monad.Logger (Logger)
import Graphics.Vty.Input.Events (Event (..), Key (..))
import Lens.Micro.Platform (Lens, Lens', makeLensesWith, use, (%=), (.=), (<&>), (^.), (^?), _Just, _head)
import RequireCallStack (RequireCallStack)
import System.NixNG.TH (duplicateRules)

data PromptItem
  = PromptItem'Freeform
      { prompt :: Text
      , response'text :: MVar' Text
      , confidential :: Bool
      }
  | PromptItem'YN
      { prompt :: Text
      , response'bool :: MVar' Bool
      }

data Prompt = Prompt
  { accumulator :: Maybe Text
  , last :: Maybe Text
  , items :: [PromptItem]
  }

makeLensesWith duplicateRules ''PromptItem
makeLensesWith duplicateRules ''Prompt

emptyPrompt :: Prompt
emptyPrompt =
  Prompt
    { accumulator = Nothing
    , last = Nothing
    , items = []
    }

promptAddItem :: Prompt -> PromptItem -> Prompt
promptAddItem prompt@Prompt{items, accumulator} item =
  prompt
    { items = items <> [item]
    , accumulator = accumulator <|> Just ""
    }

eventPrompt
  :: (Concurrent :> es, Logger :> es, RequireCallStack)
  => (forall a. Eff es a -> IO a) -> BrickEvent n e -> EventM n Prompt ()
eventPrompt _ (VtyEvent (EvKey KBS [])) = _accumulator . _Just %= T.dropEnd 1
eventPrompt _ (VtyEvent (EvKey KUp [])) =
  use _last >>= \case
    Just last' -> _accumulator .= Just last'
    Nothing -> pure ()
eventPrompt unlift (VtyEvent (EvKey KEnter [])) = do
  use _items >>= \case
    PromptItem'Freeform{response'text} : rest -> do
      accumulator <- use (_accumulator . _Just)

      liftIO . unlift $ putMVar' response'text accumulator
      _last .= Just accumulator
      _items .= rest
      if null rest
        then _accumulator .= Nothing
        else _accumulator .= Just ""
    PromptItem'YN{response'bool} : rest -> do
      response <-
        use (_accumulator . _Just) <&> T.toLower <&> \case
          "y" -> Just True
          "n" -> Just False
          _ -> Nothing
      case response of
        Just response' -> do
          liftIO . unlift $ putMVar' response'bool response'
          use _accumulator >>= (_last .=)
          _items .= rest
          if null rest
            then _accumulator .= Nothing
            else _accumulator .= Just ""
        Nothing -> pure ()
    [] -> pure ()
eventPrompt _ (VtyEvent (EvKey (KChar c) [])) = _accumulator . _Just %= (`T.snoc` c)
eventPrompt _ _ = pure ()

promptActive :: Prompt -> Bool
promptActive Prompt{items} = not $ null items

renderPromptItem :: Text -> PromptItem -> Widget n
renderPromptItem accumulator PromptItem'Freeform{prompt, confidential} =
  txt prompt
    <+> txt accumulator'
 where
  accumulator' =
    if confidential
      then T.replicate (T.length accumulator) "*"
      else accumulator
renderPromptItem accumulator PromptItem'YN{prompt} =
  txt prompt
    <+> txt accumulator

renderPrompt :: Prompt -> Widget n
renderPrompt Prompt{accumulator = Just accumulator, items = item : _} = renderPromptItem accumulator item
renderPrompt Prompt{accumulator = Nothing, items = _ : _} = error "unreachable"
renderPrompt Prompt{items = []} = emptyWidget

-- ++ maybe [] (singleton . padTop Max . makePrompt) (headMay askpass)
