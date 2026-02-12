{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Cli.Machine where

import Brick.AttrMap (AttrName)
import Brick.Types (Widget)
import Brick.Widgets.Border (hBorderWithLabel)
import Brick.Widgets.Core (
  Padding (Max, Pad),
  emptyWidget,
  fill,
  hLimit,
  hLimitPercent,
  padLeft,
  padRight,
  txt,
  txtWrap,
  vBox,
  vLimit,
  withAttr,
  (<+>),
  (<=>),
 )
import Brick.Widgets.List (GenericList, renderList)
import Control.Applicative (Alternative ((<|>)))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful.Exception (ExceptionWithContext, SomeException)
import Effectful.Process.Typed (ExitCode (..))
import Graphics.Vty.Attributes (Attr)
import Lens.Micro.Internal (At (at), Index, IxValue, Ixed (ix))
import Lens.Micro.Platform (Lens', Traversal', lens, makeLensesWith, (%~), (.~), (<&>), (^.), _1)
import System.NixNG.TH (duplicateRules)

data MachineState
  = MachineState'Evaluating
  | MachineState'Building
  | MachineState'Copying
  | MachineState'Deploying
  | MachineState'Deployed {exitCode :: ExitCode, output :: Text}
  | MachineState'Exception {exception :: ExceptionWithContext SomeException}
  deriving (Show)

data Machine = Machine
  { expanded :: Bool
  , name :: Text
  , state :: MachineState
  }

data Machines n = Machines
  { machines :: Vector Machine
  , names :: HashMap Text Int
  , selected :: Maybe Text
  }

makeLensesWith duplicateRules ''Machine
makeLensesWith duplicateRules ''Machines

type instance Index (Machines n) = Text
type instance IxValue (Machines n) = MachineState

instance Ixed (Machines n) where
  ix :: Index (Machines n) -> Traversal' (Machines n) (IxValue (Machines n))
  ix k f m@Machines{machines, names} =
    case HM.lookup k names of
      Just v -> case machines V.!? v of
        Just v'@Machine{state} -> f state <&> \v'' -> m{machines = machines V.// [(v, v'{state = v''})]}
        Nothing -> pure m
      Nothing -> pure m
  {-# INLINE ix #-}

instance At (Machines n) where
  at k f m@Machines{machines, names, selected} =
    f (mv <&> (^. _state)) <&> \r -> case (,) mv <$> r of
      Nothing -> case names HM.!? k of
        Just index -> m{machines = V.ifilter (curry ((/= index) . (^. _1))) machines, names = HM.delete k names}
        Nothing -> m
      Just (v', state) ->
        let v'default = (maybe Machine{name = k, expanded = False, state} (_state .~ state) v')
         in case names HM.!? k of
              Just index -> m{machines = machines V.// [(index, v'default)]}
              Nothing ->
                m{machines = machines `V.snoc` v'default, names = HM.insert k (V.length machines) names, selected = selected <|> Just k}
   where
    mv = HM.lookup k names >>= (machines V.!?)
  {-# INLINE at #-}

data Dir = Next | Prev

machinesSelect :: Dir -> Machines n -> Machines n
machinesSelect dir = case dir of
  Prev -> select' (-1)
  Next -> select' 1
 where
  select' _ m@Machines{selected = Nothing} = m
  select' offset m@Machines{selected = Just selected, machines, names} =
    case names HM.!? selected of
      Just index ->
        case machines V.!? (index + offset) of
          Just newMachine -> m{selected = Just (newMachine ^. _name)}
          Nothing -> m
      Nothing -> m

machinesPrev :: Machines n -> Machines n
machinesPrev = machinesSelect Prev

machinesNext :: Machines n -> Machines n
machinesNext = machinesSelect Next

emptyMachines :: Machines n
emptyMachines = Machines{machines = V.empty, names = HM.empty, selected = Nothing}

machineExpand :: Machines n -> Machines n
machineExpand m@Machines{selected = Just selected, names, machines} =
  case names HM.!? selected of
    Just index ->
      let
        machine@Machine{expanded} = machines V.! index
       in
        m{machines = machines V.// [(index, machine{expanded = not expanded})]}
    Nothing -> m
machineExpand m@Machines{selected = Nothing} = m

renderMachineState :: Bool -> MachineState -> Widget n
renderMachineState _ MachineState'Evaluating = txt "█▒▒▒▒" <+> padLeft (Pad 1) (hLimit 6 . vLimit 1 $ txt "eval" <+> fill ' ')
renderMachineState _ MachineState'Building = txt "██▒▒▒" <+> padLeft (Pad 1) (hLimit 6 . vLimit 1 $ txt "build" <+> fill ' ')
renderMachineState _ MachineState'Copying = txt "███▒▒" <+> padLeft (Pad 1) (hLimit 6 . vLimit 1 $ txt "copy" <+> fill ' ')
renderMachineState _ MachineState'Deploying = txt "████▒" <+> padLeft (Pad 1) (hLimit 6 . vLimit 1 $ txt "deploy" <+> fill ' ')
renderMachineState expanded MachineState'Deployed{exitCode, output} =
  txt "█████"
    <+> padLeft (Pad 1) (hLimit 6 . vLimit 1 $ txt "done" <+> fill ' ')
    <+> padLeft (Pad 1) (txt exitCode')
    <=> if expanded then hLimitPercent 95 (txtWrap output) else emptyWidget
 where
  exitCode' = case exitCode of
    ExitSuccess -> "exited successfully"
    ExitFailure code -> "exited with " <> T.show code
renderMachineState _ MachineState'Exception{exception} = txt $ T.show exception

renderMachine :: Machine -> Widget n
renderMachine Machine{name, state, expanded} = name' <+> state'
 where
  name' = hLimitPercent 20 . padRight Max $ txt name
  state' = padLeft (Pad 1) . padRight Max $ renderMachineState expanded state

renderMachines :: AttrName -> Machines n -> Widget n
renderMachines altListAttr (Machines{machines, selected}) =
  headerMachines
    <+> headerState
    <=> vBox machines'
 where
  bar name = case selected of
    Just selected' -> if selected' == name then txt "|" else txt " "
    Nothing -> txt " "
  headerMachines = hLimitPercent 20 . padRight (Pad 1) . hBorderWithLabel $ txt "machines"
  headerState = padLeft (Pad 1) . hBorderWithLabel $ txt "state"
  machines' =
    zip (V.toList machines) [0 :: Int ..] & map \(machine@Machine{name}, i) ->
      ( if i `mod` 2 == 0
          then withAttr altListAttr
          else
            id
      )
        $ bar name <+> renderMachine machine
