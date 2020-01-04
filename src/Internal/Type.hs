{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Internal.Type where

import Control.Lens  (makeFields)
import Data.Fin      (Fin (..))
import Data.Nat      (Nat (..))
import Data.Vec.Lazy (Vec)
import GHC.Generics  (Generic)

type NodeID = Fin

type Ends n = Vec n Nat

newtype Slime = Slime Nat
  deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

data Unit = Sprayer | Lobber deriving (Eq, Generic, Show)

newtype Order n = Order {_order :: Maybe (NodeID n)} deriving (Eq, Generic, Show)

data Dummy n = Dummy
  { _dummyEnds   :: Vec n (Ends n)
  , _dummySlime  :: Vec n Slime
  , _dummyUnits  :: Vec n (Maybe Unit)
  , _dummyOrders :: Vec n (Maybe (Order n)) }

$(makeFields ''Dummy)

type HEnds s n = HasEnds s (Vec n (Ends n))
type HSlime s n = HasSlime s (Vec n Slime)
type HUnits s n = HasUnits s (Vec n (Maybe Unit))
type HOrders s n = HasOrders s (Vec n (Order n))

