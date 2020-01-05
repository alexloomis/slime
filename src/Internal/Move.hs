module Internal.Move where

import Internal.Import
import Internal.Order  (clearAllOrders, hasOrder)

import Control.Monad (liftM2)
import Data.List     (nub)
import Data.Maybe    (isJust, mapMaybe)
import Data.Tuple    (swap)

-- |Sets all units with order Nothing to instead move to their own node.
addHolds :: (HUnits s n, HOrders s n) => s -> s
addHolds s = over orders (imap f) s
  where
    f k v = if isJust (viewAt units s k) && (not . hasOrder $ v)
      then Order $ Just k
      else v

destinations :: HOrders s n => s -> [NodeID n]
destinations = mapMaybe _order . toList . view orders

-- |Returns True if the set of destinations contains duplicates.
hasOverlap :: HOrders s n => s -> Bool
hasOverlap = liftM2 (/=) destinations (nub . destinations)

-- Maybe get a node with orders to move to n.
movingTo :: HOrders s n => s -> NodeID n -> Maybe (NodeID n)
movingTo s n = lookup (Order $ Just n) . fmap swap . toMap $ s ^. orders

newPositions :: (HUnits s n, HOrders s n) => s -> Vec n (Maybe Unit)
newPositions s = imap f . view orders $ s
  where
    f k _ = case movingTo s k of
      Nothing -> Nothing
      Just n  -> viewAt units s n

-- |Tries to execute orders.
-- If any overlaps are detected, maintains the original state.
-- Clears all orders regardless.
resolveOrders :: (HUnits s n, HOrders s n) => s -> s
resolveOrders s = if hasOverlap s'
  then clearAllOrders s
  else clearAllOrders $ set units (newPositions s') s
  where s' = addHolds s

