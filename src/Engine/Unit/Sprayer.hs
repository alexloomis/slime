{-# LANGUAGE TupleSections #-}

module Engine.Unit.Sprayer
  ( resolveSprayer
  , sprayerDamage
  ) where

import Engine.Internal.Type
import Engine.Slime
import Engine.Unit.Util

import           Control.Lens.Combinators
import           Control.Monad.State
import qualified Data.HashMap.Lazy        as HM
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as Set

sprayerDamage :: Slime
sprayerDamage = 1

zeroedNodes :: HNodes s => s -> [(Node,Slime)]
zeroedNodes = fmap (,0) . Set.toList . view nodes

sprayerTargets :: (HEdges s, HUnits s) => s -> [Node]
sprayerTargets state = concat . HM.elems
  . HM.intersection (view edges state) $ unit Sprayer state

sprayerDelta :: (HNodes s, HEdges s, HUnits s) => s -> NodeAttr Slime
sprayerDelta state = HM.fromListWith (+) $
  fmap (,sprayerDamage) (sprayerTargets state) ++ zeroedNodes state

applySprayer :: (HNodes s, HEdges s, HSlime s, HUnits s) => s -> NodeAttr Slime
applySprayer = liftM2 (HM.intersectionWith satSub) (view slime) sprayerDelta

resolveSprayer :: (HNodes s, HEdges s, HSlime s, HUnits s) => s -> s
resolveSprayer p = set slime (applySprayer p) p

