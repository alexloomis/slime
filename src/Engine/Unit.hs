module Engine.Unit
  ( resolveUnits
  , removeUnits
  , removeDeadOrders
  , resolveDeaths
  , lobberTargets
  , sprayerDamage
  , module Engine.Unit.Move
  , module Engine.Unit.Order
  ) where

import Engine.Internal.Type
import Engine.PackAttr
import Engine.Unit.Lobber
import Engine.Unit.Move
import Engine.Unit.Order
import Engine.Unit.Sprayer

import           Control.Lens.Combinators
import           Control.Monad            (liftM2)
import qualified Data.HashMap.Lazy        as HM
import           Data.Maybe               (isNothing)

resolveUnits :: (HNodes a, HEnds a, HSlime a, HUnits a) => a -> a
resolveUnits = resolveLobber . resolveSprayer

killUnits :: (HSlime s, HUnits s) => s -> NodeAttr (Maybe Unit)
killUnits = liftM2 mask
  (HM.map (const Nothing)
  . HM.filter (/= 0)
  . view slime)
  (view units)

cleanOrders :: (HUnits s, HOrders s) => s -> NodeAttr (Maybe Node)
cleanOrders = liftM2 mask
  (HM.map (const Nothing)
  . HM.filter isNothing
  . view units)
  (view orders)

removeDeadOrders :: (HUnits s, HOrders s) => s -> s
removeDeadOrders s = set orders (cleanOrders s) s

removeUnits :: (HSlime s, HUnits s) => s -> s
removeUnits s = set units (killUnits s) s

resolveDeaths :: (HSlime s, HUnits s, HOrders s) => s -> s
resolveDeaths = removeDeadOrders . removeUnits

