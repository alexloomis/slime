{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Unit.Util where

import Engine.Internal.Type

import Control.Lens.Combinators
import Data.HashMap.Lazy        as HM
import Data.HashSet             (HashSet)
import Data.Maybe               (isNothing)

unit :: HUnits a => Unit -> a -> NodeAttr (Maybe Unit)
unit u = HM.filter (== Just u) . view units

noUnits :: HUnits a => a -> HashSet Node
noUnits = HM.keysSet . HM.filter isNothing . view units

unitLocs :: HUnits a => a -> NodeAttr (Maybe Unit)
unitLocs = HM.filter (/= Nothing) . view units

