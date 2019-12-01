{-# LANGUAGE RecordWildCards #-}

module Board.Sandpile where
{-
module Board.Sandpile
  ( Sandpile(..)
  , makeSandpile
  ) where
-}

import Core.Type
import Slime.Type

import Control.Monad       (liftM3)
import Control.Monad.State
import Data.HashSet        (HashSet)

data Sandpile = Sandpile
  { nodes :: HashSet Node
  , edges :: NodeAttr [Node]
  , slime :: NodeAttr Slime
  } deriving (Show)

makeSandpile :: HasNodes a => NodeAttr [Node] -> NodeAttr Slime -> State a Sandpile
makeSandpile rawEdges rawSlime =
  liftM3 Sandpile (gets getNodes) (packAttr rawEdges) (packAttr rawSlime)

updateSPEdges :: Sandpile -> NodeAttr [Node] -> Sandpile
updateSPEdges Sandpile {..} e = Sandpile {edges = e, ..}

-- updateEdges s e = s {edges = e}
-- incrementA x@Foo{..} = x { a = succ a }

