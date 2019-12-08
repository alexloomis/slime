{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Board.MoveUnit
  ( MoveUnit
  , makeMoveUnit
  ) where

import Engine.Internal.Type
import Engine.PackAttr

import Control.Lens.Combinators
import Data.HashSet             (HashSet)

data MoveUnit = MoveUnit
  { _nodes  :: HashSet Node
  , _edges  :: NodeAttr [Node]
  , _units  :: NodeAttr (Maybe Unit)
  , _orders :: NodeAttr (Maybe Node)
  } deriving (Eq, Show)

$(makeFieldsNoPrefix ''MoveUnit)

makeMoveUnit :: HashSet Node -> NodeAttr [Node]
  -> NodeAttr (Maybe Unit) -> NodeAttr (Maybe Node) -> MoveUnit
makeMoveUnit _nodes _edges _units _orders =
  packAttr _orders
  . packAttr _units
  . packAttr _edges
  $ MoveUnit {..}

