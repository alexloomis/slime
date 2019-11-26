module Position
  ( Position(..)
  , makePosition
  ) where

import Core.Type
import Slime.Resolve
import Slime.Type

import Control.Monad (liftM3)
import Control.Monad.State
import Data.HashSet (HashSet)

data Position =
  Position
    { positionNodes :: HashSet Node
    , positionEdges :: [Edge]
    , positionSlime :: Slime
    }
  deriving (Show)

instance HasNodes Position where
  hasNodes = positionNodes

instance HasEdges Position where
  hasEdges = positionEdges

instance HasSlime Position where
  hasSlime = positionSlime

makePosition :: HasNodes a => [(Node, Node)] -> Slime -> State a Position
makePosition pairs rawSlime =
  liftM3 Position (gets hasNodes) (makeEdges pairs) (packAttr rawSlime)
