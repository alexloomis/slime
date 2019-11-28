module Position
  ( Position(..)
  , makePosition
  ) where

import Core.Type
import Slime.Type

import Control.Monad       (liftM3)
import Control.Monad.State
import Data.HashSet        (HashSet)
-- import Data.HashMap.Lazy (HashMap)

data Position = Position
  { positionNodes :: HashSet Node
  , positionEdges :: NodeAttr [Node]
  , positionSlime :: NodeAttr Slime
  } deriving (Show)

instance HasNodes Position where hasNodes = positionNodes
instance HasEdges Position where hasEdges = positionEdges
instance HasSlime Position where hasSlime = positionSlime

-- WARNING! Does not check that every node is a key for Slime,
-- nor that every node is a key for Edges,
-- nor that values for Edges correspond to real nodes.
makePosition :: HasNodes a => NodeAttr [Node] -> NodeAttr Slime -> State a Position
makePosition edges rawSlime =
  liftM3 Position (gets hasNodes) (packAttr edges) (packAttr rawSlime)

