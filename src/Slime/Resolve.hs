{-# LANGUAGE FlexibleContexts #-}

module Slime.Resolve (resSlime) where

import Core.Extra
import Core.Type
import Slime.Type

import Control.Monad.State
import Data.HashMap.Lazy

edgesFrom :: (MonadState a m, HasEdges a) => Node -> m Int
edgesFrom node = length <$> attr hasEdges node

slimePerEdge :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Int
slimePerEdge = liftM2 (liftM2 divOrZero) (attr hasSlime) edgesFrom

remainingMap :: (HasEdges a, HasSlime a) => State a (Node -> Int)
remainingMap = packState $ liftM2 (liftM2 mod) (attr hasSlime) edgesFrom

remainingSlime :: (HasEdges a, HasSlime a) => State a (NodeAttr Slime)
remainingSlime = do
  rmap <- remainingMap
  gets hasEdges >>= (return . fromList . fmap (mapToSnd rmap) . keys)
    where mapToSnd f a = (a,f a)

addedSlime :: (HasEdges a, HasSlime a) => State a (NodeAttr Slime)
addedSlime = liftM2 g (packState slimePerEdge) (gets hasEdges)
  where g s = fromListWith (+)
              . concatMap (\(a,bs) -> [(b,s a) | b <- bs]) . toList

resSlime :: (HasEdges a, HasSlime a) => State a (NodeAttr Slime)
resSlime = liftM2 (intersectionWith (+)) addedSlime remainingSlime

