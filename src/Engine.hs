module Engine
  ( -- *From Internal.Type
    NodeID
  , Ends
  , Slime (..)
  , Unit (..)
  , Order (..)
  , HasEnds (..)
  , HEnds
  , HasSlime (..)
  , HSlime
  , HasUnits (..)
  , HUnits
  , HasOrders (..)
  , HOrders
  , getEnds
  , getSlime
  , getUnits
  , getOrders
  -- *From Internal.Slime
  , resolveSlime
  , slimePerEnd
  -- *From internal.Unit
  , lobberRange
  , sprayerRange
  , sprayerDamage
  , resolveUnits
  , resolveDeaths
  -- *From Internal.Order
  , ErrCode
  , OneOrder (..)
  , toOneOrders
  , checkOrder
  , hasOrder
  , giveOrder
  , clearOrderFrom
  , clearAllOrders
  -- *From Internal.Move
  , resolveOrders
  -- *From Internal.Util
  , toMap
  ) where

import Internal.Move
import Internal.Order
import Internal.Slime
import Internal.Type
import Internal.Unit
import Internal.Util

import Control.Lens      (view)
import Data.Vector.Sized (Vector)

getEnds :: HEnds s n => s -> Vector n (Ends n)
getEnds = view ends

getSlime :: HSlime s n => s -> Vector n Slime
getSlime = view slime

getUnits :: HUnits s n => s -> Vector n (Maybe Unit)
getUnits = view units

getOrders :: HOrders s n => s -> Vector n (Order n)
getOrders = view orders

