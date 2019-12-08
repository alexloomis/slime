{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Board.Partial
  ( Sandpile
  , makeSandpile
  , StaticUnit
  , makeStaticUnit
  , MoveUnit
  , makeMoveUnit
  ) where

import Engine.Internal.Type
import Engine.PackAttr

import Control.Lens.Combinators
import Data.HashSet             (HashSet)

data Sandpile = Sandpile
  { _nodes :: HashSet Node
  , _edges :: NodeAttr [Node]
  , _slime :: NodeAttr Slime
  } deriving (Show)

data StaticUnit = StaticUnit
  { _nodes :: HashSet Node
  , _edges :: NodeAttr [Node]
  , _slime :: NodeAttr Slime
  , _units :: NodeAttr (Maybe Unit)
  } deriving (Eq, Show)

data MoveUnit = MoveUnit
  { _nodes  :: HashSet Node
  , _edges  :: NodeAttr [Node]
  , _units  :: NodeAttr (Maybe Unit)
  , _orders :: NodeAttr (Maybe Node)
  } deriving (Eq, Show)

$(makeFieldsNoPrefix ''StaticUnit)
$(makeFieldsNoPrefix ''MoveUnit)
$(makeFieldsNoPrefix ''Sandpile)

makeSandpile :: HashSet Node -> NodeAttr [Node] -> NodeAttr Slime -> Sandpile
makeSandpile _nodes _edges _slime =
  packAttr _slime . packAttr _edges $ Sandpile {..}

makeStaticUnit :: HashSet Node -> NodeAttr [Node] -> NodeAttr Slime
  -> NodeAttr (Maybe Unit) -> StaticUnit
makeStaticUnit _nodes _edges _slime _units =
  packAttr _units
  . packAttr _slime
  . packAttr _edges
  $ StaticUnit {..}

makeMoveUnit :: HashSet Node -> NodeAttr [Node]
  -> NodeAttr (Maybe Unit) -> NodeAttr (Maybe Node) -> MoveUnit
makeMoveUnit _nodes _edges _units _orders =
  packAttr _orders
  . packAttr _units
  . packAttr _edges
  $ MoveUnit {..}

