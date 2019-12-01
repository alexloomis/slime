module Slime.Resolve (resolveSlime) where
-- module Slime.Resolve where

import Core.Extra
import Core.Type
import Slime.Type

import           Control.Monad.State
import qualified Data.HashMap.Lazy   as HM
import qualified Data.HashSet        as Set

edgesFrom :: (MonadState a m, HasEdges a) => Node -> m Slime
edgesFrom node = fromIntegral . length <$> attr getEdges node

slimePerEdge :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Slime
slimePerEdge = liftM2 (liftM2 divOrZero) (attr getSlime) edgesFrom

remainingMap :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Slime
remainingMap node = do
  spe <- slimePerEdge node
  ef <- edgesFrom node
  slime <- attr getSlime node
  return $ slime - spe * ef

remainingSlime :: (HasEdges a, HasSlime a) => State a (NodeAttr Slime)
remainingSlime = do
  rmap <- packState remainingMap
  gets getEdges >>= (return . HM.fromList . fmap (mapToSnd rmap) . HM.keys)
    where mapToSnd f a = (a,f a)

addedSlime :: (HasEdges a, HasSlime a) => State a (NodeAttr Slime)
addedSlime = liftM2 g (packState slimePerEdge) (gets getEdges)
  where g s = HM.fromListWith (+)
              . concatMap (\(a,bs) -> [(b,s a) | b <- bs]) . HM.toList

addedMap :: (HasEdges a, HasSlime a) => Node -> State a Slime
addedMap n = do
  invNbrs <- gets (HM.filter (elem n) . getEdges)
  spe <- packState slimePerEdge
  return $ sum . HM.elems . HM.mapWithKey (\k v -> spe k * count n v) $ invNbrs
  where count n = fromIntegral . length . filter (== n)

resolveSlime :: (HasNodes a, HasEdges a, HasSlime a) => State a (NodeAttr Slime)
resolveSlime = do
  nodes <- gets (HM.mapWithKey const . Set.toMap . getNodes)
  added <- packState addedMap
  remaining <- packState remainingMap
  return $ HM.map (liftM2 (+) added remaining) nodes

