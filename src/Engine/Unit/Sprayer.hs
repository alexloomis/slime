{-# LANGUAGE TupleSections #-}

module Engine.Unit.Sprayer where
  -- ( resolveSprayer
  -- , sprayerDamage
  -- ) where

import Engine.Internal.Type
import Engine.Slime
import Engine.Unit.Util

import           Control.Lens.Combinators
import           Control.Monad            (liftM2)
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as Set

sprayerDamage :: Slime
sprayerDamage = 1

zeroedNodes :: HNodes s => s -> [(Node,Slime)]
zeroedNodes = fmap (,0) . Set.toList . view nodes

-- A node targeted twice should be listed twice.
sprayerTargets :: (HEnds s, HUnits s) => s -> [Node]
sprayerTargets state = concatMap HM.keys . HM.elems
  . HM.intersection (view ends state) $ unit Sprayer state

sprayerDelta :: (HNodes s, HEnds s, HUnits s) => s -> NodeAttr Slime
sprayerDelta state = HM.fromListWith (+) $
  fmap (,sprayerDamage) (sprayerTargets state) ++ zeroedNodes state

applySprayer :: (HNodes s, HEnds s, HSlime s, HUnits s) => s -> NodeAttr Slime
applySprayer = liftM2 (HM.intersectionWith satSub) (view slime) sprayerDelta

resolveSprayer :: (HNodes s, HEnds s, HSlime s, HUnits s) => s -> s
resolveSprayer p = set slime (applySprayer p) p

