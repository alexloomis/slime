module Unit.Type where

import Core.Type
import Slime.Type

import Data.HashMap.Lazy as HM

data Unit = Sprayer | Lobber deriving Eq

class HasUnits a where hasUnits :: a -> NodeAttr (Maybe Unit)

sprayerDamage :: Slime
sprayerDamage = 1

unit :: HasUnits a => Unit -> a -> NodeAttr (Maybe Unit)
unit u = HM.filter (Just u ==) . hasUnits

