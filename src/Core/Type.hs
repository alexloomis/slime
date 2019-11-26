{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Type
  ( Node(..)
  , NodeAttr
  , packAttr
  , Edge(..)
  , HasNodes(..)
  , HasEdges(..)
  , makeEdges
  ) where

import Control.Monad.State
import Data.Foldable (toList)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)

newtype Node =
  Node
    { nodeName :: Text
    }
  deriving (Eq, Hashable, IsString, Show)

type NodeAttr a = HashMap Node a

data Edge =
  Edge
    { edgeFrom :: Node
    , edgeTo :: Node
    }
  deriving (Show)

class HasNodes a where
  hasNodes :: a -> HashSet Node

class HasEdges a where
  hasEdges :: a -> [Edge]

instance HasNodes Node where
  hasNodes = Set.singleton

instance (Foldable t, HasNodes a) => HasNodes (t a) where
  hasNodes = Set.unions . fmap hasNodes . toList

instance HasEdges Edge where
  hasEdges = pure

instance (Foldable t, HasEdges a) => HasEdges (t a) where
  hasEdges = concatMap hasEdges

-- |Packs values corresponding to existing nodes.
-- Does not check that every node is a key.
packAttr :: (MonadState a m, HasNodes a) => NodeAttr b -> m (NodeAttr b)
packAttr slime = do
  nodes <- gets hasNodes
  return $ HM.intersection slime (Set.toMap nodes)

-- |@Nothing@ if either endpoint doesn't exist.
makeEdge :: HasNodes a => (Node, Node) -> State a (Maybe Edge)
makeEdge (id1, id2) = do
  ids <- gets hasNodes
  return $
    if elem id1 ids && elem id2 ids
      then Just $ Edge id1 id2
      else Nothing

-- |Returns only edges whose endpoints exist.
makeEdges :: HasNodes a => [(Node, Node)] -> State a [Edge]
makeEdges = fmap catMaybes . mapM makeEdge
