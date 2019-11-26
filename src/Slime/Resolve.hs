{-# LANGUAGE FlexibleContexts #-}

module Slime.Resolve
  ( resolveSlime
  ) where

import Core.Extra
import Core.Type
import Slime.Type

import Control.Monad.State
import Data.HashMap.Lazy (mapWithKey)

edgesTo :: (MonadState a m, HasEdges a) => Node -> m [Edge]
edgesTo iden = filter (\x -> edgeTo x == iden) <$> gets hasEdges

edgesFrom :: (MonadState a m, HasEdges a) => Node -> m [Edge]
edgesFrom iden = filter (\x -> edgeFrom x == iden) <$> gets hasEdges

slimePerEdge :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Int
slimePerEdge = liftM2 (liftM2 divOrZero) slimeAt (fmap length . edgesFrom)

slimeFrom :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Int
slimeFrom = liftM2 (liftM2 (*)) (fmap length . edgesFrom) slimePerEdge

slimeTo :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Int
slimeTo iden = edgesTo iden >>= fmap sum . mapM (slimePerEdge . edgeFrom)

deltaSlime :: (MonadState a m, HasEdges a, HasSlime a) => Node -> m Int
deltaSlime = liftM2 (liftM2 (-)) slimeTo slimeFrom

resolveSlime :: (HasEdges a, HasSlime a) => State a Slime
resolveSlime = do
  new <- stuffState deltaSlime
  mapWithKey ((+) . new) <$> gets hasSlime
