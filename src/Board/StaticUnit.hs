{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Board.StaticUnit
  ( StaticUnit
  , makeStaticUnit
  ) where

import Engine.Internal.Type
import Engine.PackAttr

import Control.Lens.Combinators
import Data.HashSet             (HashSet)

data StaticUnit = StaticUnit
  { _nodes :: HashSet Node
  , _edges :: NodeAttr [Node]
  , _slime :: NodeAttr Slime
  , _units :: NodeAttr (Maybe Unit)
  } deriving (Eq, Show)

$(makeFieldsNoPrefix ''StaticUnit)

makeStaticUnit :: HashSet Node -> NodeAttr [Node] -> NodeAttr Slime
  -> NodeAttr (Maybe Unit) -> StaticUnit
makeStaticUnit _nodes _edges _slime _units =
  packAttr _units
  . packAttr _slime
  . packAttr _edges
  $ StaticUnit {..}

