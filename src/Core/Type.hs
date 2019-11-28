{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Type
  ( Node(..)
  , NodeAttr
  , packAttr
  , attr
  , HasNodes(..)
  , HasEdges(..)
  ) where

import           Control.Monad.State
import           Data.Foldable       (toList)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Lazy   (HashMap, (!))
import qualified Data.HashMap.Lazy   as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.String         (IsString)
import           Data.Text           (Text)

newtype Node = Node {nodeName :: Text}
  deriving (Eq, Hashable, IsString, Show)

type NodeAttr = HashMap Node

class HasNodes a where hasNodes :: a -> HashSet Node
class HasEdges a where hasEdges :: a -> NodeAttr [Node]

instance HasNodes Node where hasNodes = Set.singleton
instance (Foldable t, HasNodes a) => HasNodes (t a) where
  hasNodes = Set.unions . fmap hasNodes . toList
instance HasNodes (NodeAttr a) where hasNodes = HM.keysSet
instance HasEdges (NodeAttr [Node]) where hasEdges = id

-- |Packs values corresponding to existing nodes.
-- Does not check that every node is a key.
packAttr :: (MonadState a m, HasNodes a) => NodeAttr b -> m (NodeAttr b)
packAttr a = do
  nodes <- gets hasNodes
  return $ HM.intersection a (Set.toMap nodes)

-- Specialises to, e.g.,
-- attrAt :: (a -> NodeAttr Slime) -> Node -> State a Slime
-- so in
-- a <- attr hasSlime node,
-- a :: Slime.
attr :: (Eq k, Hashable k, MonadState a m)
  => (a -> HashMap k b) -> k -> m b
attr projection node = flip (!) node <$> gets projection

