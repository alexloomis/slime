{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Board.Sandpile
  ( Sandpile
  , makeSandpile
  ) where

import Engine.Internal.Type
import Engine.PackAttr

import Control.Lens.Combinators
import Data.HashSet             (HashSet)

data Sandpile = Sandpile
  { _nodes :: HashSet Node
  , _ends :: NodeAttr [Node]
  , _slime :: NodeAttr Slime
  } deriving (Show)

$(makeFieldsNoPrefix ''Sandpile)

makeSandpile :: HashSet Node -> NodeAttr [Node] -> NodeAttr Slime -> Sandpile
makeSandpile _nodes _ends _slime =
  packAttr _slime . packAttr _ends $ Sandpile {..}


