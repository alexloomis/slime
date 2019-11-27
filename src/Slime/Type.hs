module Slime.Type where

import Core.Type

import Control.Monad.State
import Data.HashMap.Lazy   (difference, intersectionWith, (!))

type Slime = Int

class HasSlime a where hasSlime :: a -> NodeAttr Slime

slimeAt :: (MonadState a m, HasSlime a) => Node -> m Int
slimeAt node = flip (!) node <$> gets hasSlime

