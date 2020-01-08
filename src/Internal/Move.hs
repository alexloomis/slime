module Internal.Move where

import Internal.Import
import Internal.Order  (clearAllOrders, clearOrderFrom, hasOrder)

import Control.Monad (liftM2)
import Data.List     (nub, (\\))
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

-- |Get nodes with orders to move to n.
movingTo :: HOrders s n => s -> NodeID n -> [NodeID n]
movingTo s n = fmap snd . filter (\x -> fst x == (Order $ Just n))
  . fmap swap . toMap $ s ^. orders

-- |Which nodes contain units moving to a conflicted destination.
destConflicts :: HOrders s n => s -> [NodeID n]
destConflicts s = concatMap (movingTo s) . nub
  . liftM2 (\\) id nub . destinations $ s

-- |Clears conflicting orders, including holds.
clearConflicts :: HOrders s n => s -> s
clearConflicts s = foldr clearOrderFrom s $ destConflicts s

newPositions :: (HUnits s n, HOrders s n) => s -> Vector n (Maybe Unit)
newPositions s = imap f . view orders $ s
  where
    f k _ = case movingTo s k of
      []  -> Nothing
      [n] -> viewAt units s n
      _   -> Nothing -- Behavior undefined

-- |Tries to execute orders.
-- If any overlaps are detected, maintains the original state.
-- Clears all orders regardless.
resolveOrders :: (HUnits s n, HOrders s n) => s -> s
resolveOrders s = clearAllOrders $ set units (newPositions s') s
  where s' = addHolds . clearConflicts . addHolds $ s

