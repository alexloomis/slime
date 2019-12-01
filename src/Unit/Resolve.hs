{-# LANGUAGE TupleSections #-}

module Unit.Resolve where

import Core.Extra
import Core.Extra.MaybeNat
import Core.Type
import Slime.Type
import Unit.Type

import           Control.Monad.State
import qualified Data.HashMap.Lazy   as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.List.Extra     (maximumOn)

sprayerDelta :: (MonadState a m, HasNodes a, HasEdges a, HasUnits a)
  => m (NodeAttr Slime)
sprayerDelta = do
  edges <- gets getEdges
  units <- gets (unit Sprayer)
  nodes <- gets getNodes
  let targets = concat . HM.elems . HM.intersection edges $ units
  let damage = fmap (,sprayerDamage) targets ++ fmap (,0) (Set.toList nodes)
  return $ HM.fromListWith (+) damage

applySprayer :: (MonadState a m, HasNodes a, HasEdges a, HasSlime a, HasUnits a)
  => m (NodeAttr Slime)
applySprayer = do
  delta <- sprayerDelta
  slime <- gets getSlime
  return $ HM.intersectionWith f slime delta
  where f (Slime a) (Slime b) = Slime $ satSub a b

maxNeighbors :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m [Node]
maxNeighbors node = do
  nbrs <- attr getEdges node
  slime <- gets getSlime
  return $ maximaOn (\n -> HM.lookupDefault 0 n slime) nbrs
  where
    maximaOn _ [] = []
    maximaOn f xs = filter (\x -> f x == f (maximumOn f xs)) xs

lobberTargets :: (HasNodes a, HasEdges a, HasSlime a, HasUnits a)
  => State a (HashSet Node)
lobberTargets = do
  pos <- gets (getNodes . unit Lobber)
  maxNs <- packState maxNeighbors
  return $ Set.fromList . concatMap maxNs . Set.toList $ pos

applyLobber :: (HasNodes a, HasEdges a, HasSlime a, HasUnits a)
  => State a (NodeAttr Slime)
applyLobber = do
  slime <- gets getSlime
  targets <- lobberTargets
  let toZero = HM.map (const 0) . Set.toMap $ targets
  return $ HM.union toZero slime

