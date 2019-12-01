{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Unit.Type where

import Core.Type
import Slime.Type

import Control.Monad.State (MonadState)
import Data.HashMap.Lazy   as HM
import GHC.Records         (HasField (..))

data Unit = Sprayer | Lobber deriving (Eq, Show)

instance (MonadState r m, HasNodes r) => SafePack m (Maybe Unit)

type HasUnits a = HasField "units" a (NodeAttr (Maybe Unit))

getUnits :: HasUnits a => a -> NodeAttr (Maybe Unit)
getUnits = getField @"units"

sprayerDamage :: Slime
sprayerDamage = 1

unit :: HasUnits a => Unit -> a -> NodeAttr (Maybe Unit)
unit u = HM.filter (Just u ==) . getUnits

