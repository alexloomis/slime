{-# LANGUAGE TemplateHaskell #-}

module Board.StaticUnit
  ( StaticUnit (..)
  , makeStaticUnit
  ) where

import Core.Type
import Core.Util

import Control.Lens.Combinators
import Control.Monad            (liftM4)
import Control.Monad.State
import Data.HashSet             (HashSet)

data StaticUnit = StaticUnit
  { _nodes :: HashSet Node
  , _edges :: NodeAttr [Node]
  , _slime :: NodeAttr Slime
  , _units :: NodeAttr (Maybe Unit)
  } deriving (Show)

$(makeFieldsNoPrefix ''StaticUnit)

makeStaticUnit :: (MonadState r m, HNodes r)
  => NodeAttr [Node] -> NodeAttr Slime -> NodeAttr (Maybe Unit) -> m StaticUnit
makeStaticUnit rawEdges rawSlime rawUnit = liftM4 StaticUnit (gets $ view nodes)
  (packAttr rawEdges) (packAttr rawSlime) (packAttr rawUnit)

