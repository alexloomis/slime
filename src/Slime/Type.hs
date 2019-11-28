module Slime.Type where

import Core.Extra.MaybeNat
import Core.Type

import Control.Monad.State
import Data.HashMap.Lazy   ((!))

type Slime = MaybeNat

class HasSlime a where hasSlime :: a -> NodeAttr Slime

slimeAt :: (MonadState a m, HasSlime a) => Node -> m Slime
slimeAt node = flip (!) node <$> gets hasSlime

