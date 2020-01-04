module Internal.Unit
  ( module Internal.Unit.Lobber
  , module Internal.Unit.Sprayer
  , resolveUnits
  , resolveDeaths
  ) where

import Internal.Import
import Internal.Unit.Lobber
import Internal.Unit.Sprayer

import Data.Maybe (isNothing)

resolveUnits :: (HEnds s n, HSlime s n, HUnits s n) => s -> s
resolveUnits = resolveLobber . resolveSprayer

killUnits :: (HSlime s n, HUnits s n) => s -> s
killUnits s = over units (imap f) s
  where
    f k v = if viewAt slime s k == 0 then v else Nothing

cleanOrders :: (HUnits s n, HOrders s n) => s -> s
cleanOrders s = over orders (imap f) s
  where
    f k v = if isNothing $ viewAt units s k then Order Nothing else v

resolveDeaths :: (HSlime s n, HUnits s n, HOrders s n) => s -> s
resolveDeaths = cleanOrders . killUnits

