{-# LANGUAGE FunctionalDependencies #-}

module Cli.ScrollableList where

import Brick.Main (lookupExtent)
import Brick.Types (BrickEvent (VtyEvent), EventM, Extent (Extent), Size (..), Widget (..))
import Brick.Widgets.Core (reportExtent, vBox)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.Function ((&))
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Monad.Logger (Logger, logDebugN)
import Graphics.Vty.Input (Event (EvKey), Key (KPageDown, KPageUp))
import Graphics.Vty.Input.Events (Key (KDown, KLeft, KUp))
import Lens.Micro.Platform (makeLensesWith, use, (%=), (%~), (.~), (^.))
import RequireCallStack (RequireCallStack)
import System.NixNG.TH (duplicateRules)

data ScrollableListName

data ScrollableList a = ScrollableList
  { items :: Vector a
  , offset :: Int
  }
makeLensesWith duplicateRules ''ScrollableList

empty :: ScrollableList a
empty = ScrollableList{items = V.empty, offset = 0}

addItem :: a -> ScrollableList a -> ScrollableList a
addItem item sl = sl & _items %~ (`V.snoc` item) & if (sl ^. _offset) /= 0 then _offset %~ (+ 1) else id

data PageDirection = PageDown | PageUp

scrollPage :: PageDirection -> Maybe (Extent n) -> ScrollableList a -> ScrollableList a
scrollPage _ Nothing = id
scrollPage pageDirection (Just (Extent _ _ (_, height))) = _offset %~ (max 0 . (+ amount))
 where
  amount = case pageDirection of
    PageUp -> 1 * height
    PageDown -> -1 * height

handleEvent
  :: (Eq n, Logger :> es, RequireCallStack)
  => n -> (forall _a. Eff es _a -> IO _a) -> BrickEvent n m -> EventM n (ScrollableList a) ()
handleEvent _ unlift (VtyEvent (EvKey KUp [])) = do
  use _items >>= \items -> _offset %= (min (length items) . (+ (-1)))
  offset <- use _offset
  liftIO . unlift $ logDebugN ("scrolled to " <> T.show offset)
handleEvent _ unlift (VtyEvent (EvKey KDown [])) = do
  _offset %= (max 0 . (+ 1))
  offset <- use _offset
  liftIO . unlift $ logDebugN ("scrolled to " <> T.show offset)
handleEvent name _ (VtyEvent (EvKey KPageUp [])) = do
  mExtent <- lookupExtent name

  modify (scrollPage PageUp mExtent)
handleEvent name _ (VtyEvent (EvKey KPageDown [])) = do
  mExtent <- lookupExtent name

  modify (scrollPage PageDown mExtent)
handleEvent _ _ _ = pure ()

render :: (Ord n) => n -> (a -> Widget n) -> ScrollableList a -> Widget n
render name renderItem ScrollableList{items, offset} =
  reportExtent name $
    vBox (V.toList . V.drop offset . V.map renderItem $ items)
