-- module Slime.Resolve (resolveSlime) where
module Slime.Resolve where

import Core.Type
import Core.Util

import           Control.Lens.Combinators
import           Control.Monad.State
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as Set

edgesFrom :: (MonadState a m, HEdges a) => Node -> m Slime
edgesFrom node = fromIntegral . length <$> attr (view edges) node

slimePerEdge :: (MonadState a m, HEdges a, HSlime a) => Node -> m Slime
slimePerEdge = liftM2 (liftM2 divOrZero) (attr (view slime)) edgesFrom

remainingMap :: (MonadState a m, HEdges a, HSlime a) => Node -> m Slime
remainingMap node = do
  spe <- slimePerEdge node
  ef <- edgesFrom node
  s <- attr (view slime) node
  return $ s - spe * ef

remainingSlime :: (HEdges a, HSlime a) => State a (NodeAttr Slime)
remainingSlime = do
  remMap <- packState remainingMap
  gets (view edges) >>= (return . HM.fromList . fmap (mapToSnd remMap) . HM.keys)
    where mapToSnd f a = (a,f a)

addedSlime :: (HEdges a, HSlime a) => State a (NodeAttr Slime)
addedSlime = liftM2 g (packState slimePerEdge) (gets (view edges))
  where g s = HM.fromListWith (+)
              . concatMap (\(a,bs) -> [(b,s a) | b <- bs]) . HM.toList

addedMap :: (HEdges a, HSlime a) => Node -> State a Slime
addedMap n = do
  invNbrs <- gets (HM.filter (elem n) . view edges)
  spe <- packState slimePerEdge
  return $ sum . HM.elems . HM.mapWithKey (\k v -> spe k * count n v) $ invNbrs
  where count k = fromIntegral . length . filter (== k)

resolveSlime :: (HNodes a, HEdges a, HSlime a) => State a (NodeAttr Slime)
resolveSlime = do
  ns <- gets (HM.mapWithKey const . Set.toMap . view nodes)
  added <- packState addedMap
  remaining <- packState remainingMap
  return $ HM.map (liftM2 (+) added remaining) ns

