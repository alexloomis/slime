{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Unit.Util where

import Core.Type

import Control.Lens.Combinators
import Data.HashMap.Lazy        as HM

sprayerDamage :: Slime
sprayerDamage = 1

unit :: HUnits a => Unit -> a -> NodeAttr (Maybe Unit)
unit u = HM.filter (Just u ==) . view units

