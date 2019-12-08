module Engine.Unit.Move
  ( newPositions
  , resolveOrders
  ) where

import Engine.Internal.Type
import Engine.Internal.Util
import Engine.Unit.Order
import Engine.Unit.Util

import           Control.Lens.Combinators
import           Control.Monad            (liftM2)
import qualified Data.HashMap.Lazy        as HM
import           Data.List                (nub)
import           Data.Maybe               (catMaybes, isNothing, mapMaybe)

-- |Sets all units with order Nothing to instead move to their own node.
nothingToHold :: (HNodes s, HUnits s, HOrders s) => s -> NodeAttr (Maybe Node)
nothingToHold s = HM.mapWithKey (const . Just)
  $ HM.intersection (HM.filter isNothing . view orders $ s) (unitLocs s)

-- |Applies nothingToHold to a state.
addHolds :: (HNodes s, HUnits s, HOrders s) => s -> s
addHolds s = over orders (HM.union $ nothingToHold s) s

-- |Returns True if the set of destinations contains duplicates.
hasOverlap :: HOrders s => s -> Bool
hasOverlap = liftM2 (/=) (catMaybes . HM.elems . view orders)
  (nub . catMaybes . HM.elems . view orders)

newPositions :: (HNodes s, HUnits s, HOrders s) => s -> NodeAttr (Maybe Unit)
newPositions s =
  (HM.fromList . mapMaybe f . HM.toList . view orders . addHolds $ s)
  `HM.union` HM.map (const Nothing) (view units s)
  where
    f (n1, Just n2) = Just (n2, valOrDefault units n1 s)
    f _             = Nothing

-- |Tries to execute every command.
-- If any overlaps are detected, maintain original state.
-- Clears all orders regardless.
resolveOrders :: (HNodes s, HUnits s, HOrders s) => s -> s
resolveOrders s = if hasOverlap . addHolds $ s
  then clearAllOrders s
  else clearAllOrders $ set units (newPositions s) s

