module Engine.Unit.Lobber where

import Engine.Internal.Type
import Engine.Internal.Util
import Engine.Unit.Util

import           Control.Lens.Combinators
import           Control.Monad.State
import qualified Data.HashMap.Lazy        as HM
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as Set
import           Data.List.Extra          (maximumOn)

maxNeighbors :: (HEdges s, HSlime s) => s -> Node -> [Node]
maxNeighbors state node =
  maximaOn (\n -> HM.lookupDefault 0 n $ view slime state) $ val edges node state
  where
    maximaOn _ [] = []
    maximaOn f xs = filter (\x -> f x == f (maximumOn f xs)) xs

lobberTargets :: (HNodes s, HEdges s, HSlime s, HUnits s)
  => s -> HashSet Node
lobberTargets state =
  Set.fromList . concatMap (maxNeighbors state) . Set.toList
  $ HM.keysSet . unit Lobber $ state

applyLobber :: (HNodes s, HEdges s, HSlime s, HUnits s)
  => s -> NodeAttr Slime
applyLobber = liftM2 HM.union
  (HM.map (const 0) . Set.toMap . lobberTargets)
  (view slime)

resolveLobber :: (HNodes a, HEdges a, HSlime a, HUnits a) => a -> a
resolveLobber p = set slime (applyLobber p) p

