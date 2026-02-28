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
import Brick.Widgets.Core (Padding (Pad), padRight, reportExtent, txt, vBox, (<+>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (trace)
import Effectful (Eff, (:>))
import Effectful.Monad.Logger (Logger, logDebugN)
import Graphics.Vty.Input (Event (EvKey), Key (KPageDown, KPageUp))
import Graphics.Vty.Input.Events (Key (KDown, KLeft, KUp))
import Lens.Micro.Platform (at, makeLensesWith, mapped, use, (%=), (%~), (.~), (^.), _1, _2, _Just)
import RequireCallStack (RequireCallStack)
import System.NixNG.TH (duplicateRules)

data ScrollableListName

data ScrollableList k a = ScrollableList
  { items :: HashMap k (Vector a)
  , allItems :: Vector (k, a)
  , offset :: Int
  }
makeLensesWith duplicateRules ''ScrollableList

empty :: ScrollableList k a
empty = ScrollableList{items = HM.empty, offset = 0, allItems = V.empty}

addItem :: (Hashable k) => k -> a -> ScrollableList k a -> ScrollableList k a
addItem key item sl =
  sl
    & _items . at key %~ (Just . (`V.snoc` item) . maybe V.empty id)
    & _allItems %~ (`V.snoc` (key, item))
    & if (sl ^. _offset) /= 0 then _offset %~ (+ 1) else id

data PageDirection = PageDown | PageUp

scrollPage :: PageDirection -> Int -> ScrollableList k a -> ScrollableList k a
scrollPage _ 0 = id
scrollPage pageDirection height = scroll height amount
 where
  amount = case pageDirection of
    PageUp -> 1 * height
    PageDown -> -1 * height

scroll :: Int -> Int -> ScrollableList k a -> ScrollableList k a
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
  items = list ^. _allItems

handleEvent
  :: (Eq n, Logger :> es, RequireCallStack)
  => n -> (forall _a. Eff es _a -> IO _a) -> BrickEvent n m -> EventM n (ScrollableList k a) ()
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

render :: (Ord n) => n -> (a -> Widget n) -> Maybe Text -> ScrollableList Text a -> Widget n
render name renderItem key ScrollableList{items, offset, allItems} =
  Widget
    { hSize = Greedy
    , vSize = Greedy
    , render = do
        c <- getContext
        Brick.render
          . reportExtent name
          . vBox
          . V.toList
          $ case key of
            Just key' ->
              V.map renderItem
                . takeEnd (c ^. availHeightL)
                . dropEnd offset
                $ maybe V.empty id (items ^. at key')
            Nothing ->
              ( mapped
                  %~ ( (\(k, v) -> padRight (Pad (machinePad - T.length k)) (txt "<" <+> txt k <+> txt "> ") <+> v)
                         . (& (_2 %~ renderItem))
                     )
              )
                . takeEnd (c ^. availHeightL)
                . dropEnd offset
                $ allItems
    }
 where
  machinePad = case HM.keys items of
    [] -> 0
    keys -> maximum (map T.length keys)
