{-# LANGUAGE FunctionalDependencies #-}

module Cli.ScrollableList where

import Brick.Main (lookupExtent)
import Brick.Types (
  BrickEvent (VtyEvent),
  EventM,
  Extent (Extent),
  Size (..),
  Widget (..),
  availHeightL,
  extentName,
  extentSize,
  getContext,
 )
import Brick.Types qualified as Brick
import Brick.Widgets.Core (reportExtent, vBox)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (trace)
import Effectful (Eff, (:>))
import Effectful.Monad.Logger (Logger, logDebugN)
import Graphics.Vty.Input (Event (EvKey), Key (KPageDown, KPageUp))
import Graphics.Vty.Input.Events (Key (KDown, KLeft, KUp))
import Lens.Micro.Platform (makeLensesWith, use, (%=), (%~), (.~), (^.), _2)
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

scrollPage :: PageDirection -> Int -> ScrollableList a -> ScrollableList a
scrollPage _ 0 = id
scrollPage pageDirection height = scroll height amount
 where
  amount = case pageDirection of
    PageUp -> 1 * height
    PageDown -> -1 * height

scroll :: Int -> Int -> ScrollableList a -> ScrollableList a
scroll height amount list
  | amount < 0 =
      list
        { offset = max 0 (offset + amount)
        }
  | amount > 0 =
      list
        { offset = min (length items - height) (offset + amount)
        }
  | otherwise = list
 where
  offset = list ^. _offset
  items = list ^. _items

handleEvent
  :: (Eq n, Logger :> es, RequireCallStack)
  => n -> (forall _a. Eff es _a -> IO _a) -> BrickEvent n m -> EventM n (ScrollableList a) ()
handleEvent name _ (VtyEvent (EvKey KDown [])) = do
  height <- lookupExtent name <&> maybe 0 ((^. _2) . extentSize)
  modify $ scroll height (-1)
handleEvent name _ (VtyEvent (EvKey KUp [])) = do
  height <- lookupExtent name <&> maybe 0 ((^. _2) . extentSize)
  modify $ scroll height 1
handleEvent name _ (VtyEvent (EvKey KPageUp [])) = do
  height <- lookupExtent name <&> maybe 0 ((^. _2) . extentSize)

  modify (scrollPage PageUp height)
handleEvent name _ (VtyEvent (EvKey KPageDown [])) = do
  height <- lookupExtent name <&> maybe 0 ((^. _2) . extentSize)

  modify (scrollPage PageDown height)
handleEvent _ _ _ = pure ()

takeEnd :: Int -> Vector a -> Vector a
takeEnd l v = V.drop (V.length v - l) v

dropEnd :: Int -> Vector a -> Vector a
dropEnd l v = V.take (V.length v - l) v

render :: (Ord n) => n -> (a -> Widget n) -> ScrollableList a -> Widget n
render name renderItem ScrollableList{items, offset} =
  Widget
    { hSize = Greedy
    , vSize = Greedy
    , render = do
        c <- getContext
        Brick.render
          . reportExtent name
          . vBox
          . V.toList
          . V.map renderItem
          . takeEnd (c ^. availHeightL)
          . dropEnd offset
          $ items
    }
