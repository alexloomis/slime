{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Board.Sandpile
  ( Sandpile
  , makeSandpile
  ) where

import Engine.Internal.Type
import Engine.Internal.Util

import Control.Lens.Combinators
import Control.Monad            (liftM3)
import Control.Monad.State
import Data.HashSet             (HashSet)

data Sandpile = Sandpile
  { _nodes :: HashSet Node
  , _edges :: NodeAttr [Node]
  , _slime :: NodeAttr Slime
  } deriving (Show)

$(makeFieldsNoPrefix ''Sandpile)

makeSandpile :: HashSet Node -> NodeAttr [Node] -> NodeAttr Slime -> Sandpile
makeSandpile _nodes _edges _slime =
  packAttr _slime . packAttr _edges $ Sandpile {..}


