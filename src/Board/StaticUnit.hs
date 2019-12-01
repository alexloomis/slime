module Board.StaticUnit
  ( StaticUnit (..)
  , makeStaticUnit
  ) where

import Core.Type
import Slime.Type
import Unit.Type

import Control.Monad       (liftM4)
import Control.Monad.State
import Data.HashSet        (HashSet)

data StaticUnit = StaticUnit
  { nodes :: HashSet Node
  , edges :: NodeAttr [Node]
  , slime :: NodeAttr Slime
  , units :: NodeAttr (Maybe Unit)
  } deriving (Show)

makeStaticUnit :: HasNodes a => NodeAttr [Node] -> NodeAttr Slime
  -> NodeAttr (Maybe Unit) -> State a StaticUnit
makeStaticUnit rawEdges rawSlime rawUnit = liftM4 StaticUnit (gets getNodes)
  (packAttr rawEdges) (packAttr rawSlime) (packAttr rawUnit)

