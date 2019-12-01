{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Slime.Type where

import Core.Extra.MaybeNat
import Core.Type

import Control.Monad.State (MonadState)
import Data.Default.Class  (Default (..))
import GHC.Records         (HasField (..))

newtype Slime = Slime MaybeNat
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Read)

instance Default Slime where def = Slime 0

instance (MonadState r m, HasNodes r) => SafePack m Slime

type HasSlime r = HasField "slime" r (NodeAttr Slime)

getSlime :: HasSlime r => r -> NodeAttr Slime
getSlime = getField @"slime"

