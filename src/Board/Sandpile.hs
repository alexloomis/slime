{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Board.Sandpile where
{-
module Board.Sandpile
  ( Sandpile(..)
  , makeSandpile
  ) where
-}

import Core.Type
import Core.Util

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

makeSandpile :: (MonadState r m, HasNodes r (HashSet Node))
  => NodeAttr [Node] -> NodeAttr Slime -> m Sandpile
makeSandpile rawEdges rawSlime =
  liftM3 Sandpile (gets $ view nodes) (packAttr rawEdges) (packAttr rawSlime)

