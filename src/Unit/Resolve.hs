{-# LANGUAGE TupleSections #-}

module Unit.Resolve where

import Core.Type
import Core.Util
import Core.Util.MaybeNat
import Unit.Util

import           Control.Lens.Combinators
import           Control.Monad.State
import qualified Data.HashMap.Lazy        as HM
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as Set
import           Data.List.Extra          (maximumOn)

sprayerDelta :: (MonadState a m, HNodes a, HEdges a, HUnits a)
  => m (NodeAttr Slime)
sprayerDelta = do
  edges <- gets (view edges)
  units <- gets (unit Sprayer)
  nodes <- gets (view nodes)
  let targets = concat . HM.elems . HM.intersection edges $ units
  let damage = fmap (,sprayerDamage) targets ++ fmap (,0) (Set.toList nodes)
  return $ HM.fromListWith (+) damage

applySprayer :: (MonadState a m, HNodes a, HEdges a, HSlime a, HUnits a)
  => m (NodeAttr Slime)
applySprayer = do
  delta <- sprayerDelta
  slime <- gets (view slime)
  return $ HM.intersectionWith f slime delta
  where f (Slime a) (Slime b) = Slime $ satSub a b

maxNeighbors :: (MonadState a m, HEdges a, HSlime a) => Node -> m [Node]
maxNeighbors node = do
  nbrs <- attr (view edges) node
  slime <- gets (view slime)
  return $ maximaOn (\n -> HM.lookupDefault 0 n slime) nbrs
  where
    maximaOn _ [] = []
    maximaOn f xs = filter (\x -> f x == f (maximumOn f xs)) xs

lobberTargets :: (HNodes a, HEdges a, HSlime a, HUnits a)
  => State a (HashSet Node)
lobberTargets = do
  pos <- gets (HM.keysSet . unit Lobber)
  maxNs <- packState maxNeighbors
  return $ Set.fromList . concatMap maxNs . Set.toList $ pos

applyLobber :: (HNodes a, HEdges a, HSlime a, HUnits a)
  => State a (NodeAttr Slime)
applyLobber = do
  slime <- gets (view slime)
  targets <- lobberTargets
  let toZero = HM.map (const 0) . Set.toMap $ targets
  return $ HM.union toZero slime

