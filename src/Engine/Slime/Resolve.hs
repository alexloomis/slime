module Engine.Slime.Resolve
  ( slimePerEdge
  , newSlime
  , resolveSlime
  ) where

import Engine.Internal.Type
import Engine.Internal.Util

import           Control.Lens.Combinators
import           Control.Monad.State
import qualified Data.HashMap.Lazy        as HM

numEdgesFrom :: HEdges s => Node -> s -> Slime
numEdgesFrom node = fromIntegral . length . val edges node

slimePerEdge :: (HEdges s, HSlime s) => Node -> s -> Slime
slimePerEdge = (liftM2 . liftM2) divOrZero (val slime) numEdgesFrom

remainingMap :: (HEdges s, HSlime s) => Node -> s -> Slime
remainingMap = (liftM3 . liftM3)
  (\a b c -> a - b * c) (val slime) slimePerEdge numEdgesFrom

remainingSlime :: (HEdges s, HSlime s) => s -> NodeAttr Slime
remainingSlime s = HM.fromList
  . fmap (mapToSnd (`remainingMap` s)) . HM.keys . view edges $ s
    where mapToSnd f a = (a,f a)

addedSlime :: (HEdges s, HSlime s) => s -> NodeAttr Slime
addedSlime s = HM.fromListWith (+) . fmap f
  . concatMap sequence . HM.toList . view edges  $ s
  where f (a,b) = (b, slimePerEdge a s)

newSlime :: (HEdges s, HSlime s) => s -> NodeAttr Slime
newSlime = liftM2 (HM.unionWith (+)) addedSlime remainingSlime

resolveSlime :: (HEdges s, HSlime s) => s -> s
resolveSlime s = set slime (newSlime s) s

