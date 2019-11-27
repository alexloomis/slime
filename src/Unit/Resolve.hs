{-# LANGUAGE TupleSections #-}
module Unit.Resolve where

import Core.Type
import Slime.Type
import Unit.Type

import           Control.Monad.State
import qualified Data.HashMap.Lazy   as HM
import qualified Data.HashSet        as Set
import           Data.List.Extra     (maximumOn)

sprayerDelta :: (MonadState a m, HasNodes a, HasEdges a, HasUnits a)
  => m (NodeAttr Slime)
sprayerDelta = do
  edges <- gets hasEdges
  units <- gets (unit Sprayer)
  nodes <- gets hasNodes
  let targets = concat . HM.elems . HM.intersection edges $ units
  let damage = fmap (,sprayerDamage) targets ++ fmap (,0) (Set.toList nodes)
  return $ HM.fromListWith (+) damage

-- resSprayerActn ::
resSprayerActn = undefined

maxNeighbor :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Node
maxNeighbor node = do
  nbrs <- attr hasEdges node
  slime <- gets hasSlime
  return $ maximumOn (\n -> HM.lookupDefault 0 n slime) nbrs

resLobberActn = undefined

