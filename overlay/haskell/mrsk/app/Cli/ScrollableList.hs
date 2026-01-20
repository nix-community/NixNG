{-# LANGUAGE FunctionalDependencies #-}

module Cli.ScrollableList where

import Brick.Types (BrickEvent (VtyEvent), EventM, Size (..), Widget (..))
import Brick.Widgets.Core (vBox)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Monad.Logger (Logger, logDebugN)
import Graphics.Vty.Input (Event (EvKey), Key (KPageDown, KPageUp))
import Graphics.Vty.Input.Events (Key (KDown, KLeft, KUp))
import Lens.Micro.Platform (makeLensesWith, use, (%=), (%~))
import RequireCallStack (RequireCallStack)
import System.NixNG.TH (duplicateRules)

data ScrollableList a = ScrollableList
  { items :: [a]
  , offset :: Int
  }
makeLensesWith duplicateRules ''ScrollableList

empty :: ScrollableList a
empty = ScrollableList{items = [], offset = 0}

addItem :: a -> ScrollableList a -> ScrollableList a
addItem item sl = sl & _items %~ (item :)

handleEvent
  :: (Logger :> es, RequireCallStack) => (forall _a. Eff es _a -> IO _a) -> BrickEvent n m -> EventM n (ScrollableList a) ()
handleEvent _ (VtyEvent (EvKey KUp [])) = _offset %= (max 0 . (+ 1))
handleEvent _ (VtyEvent (EvKey KDown [])) = use _items >>= \items -> _offset %= (min (length items) . (+ (-1)))
handleEvent _ (VtyEvent (EvKey KPageUp [])) = _offset %= (max 0 . (+ 10))
handleEvent _ (VtyEvent (EvKey KPageDown [])) = _offset %= (max 0 . (+ (-10)))
handleEvent _ _ = pure ()

render :: (a -> Widget n) -> ScrollableList a -> Widget n
render renderItem ScrollableList{items, offset} =
  vBox (drop offset (map renderItem items))
