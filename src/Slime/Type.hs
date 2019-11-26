module Slime.Type where

import Core.Type

import Control.Monad.State
import Data.HashMap.Lazy ((!), intersectionWith, difference)

type Slime = NodeAttr Int

class HasSlime a where
  hasSlime :: a -> Slime

slimeAt :: (MonadState a m, HasSlime a) => Node -> m Int
slimeAt node = flip (!) node <$> gets hasSlime
