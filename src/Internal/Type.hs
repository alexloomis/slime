{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Internal.Type where

import Control.Lens      (makeFields)
import Data.Finite       (Finite)
import Data.Vector.Sized (Vector)
import GHC.Generics      (Generic)
import Numeric.Natural   (Natural)

type NodeID = Finite

type Ends n = Vector n Natural

newtype Slime = Slime Natural
  deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

data Unit = Lobber | Sprayer deriving (Eq, Generic, Ord, Show)

newtype Order n = Order {_order :: Maybe (NodeID n)} deriving (Eq, Generic, Show)

data Dummy n = Dummy
  { _dummyEnds   :: Vector n (Ends n)
  , _dummySlime  :: Vector n Slime
  , _dummyUnits  :: Vector n (Maybe Unit)
  , _dummyOrders :: Vector n (Maybe (Order n)) }

$(makeFields ''Dummy)

type HEnds s n = HasEnds s (Vector n (Ends n))
type HSlime s n = HasSlime s (Vector n Slime)
type HUnits s n = HasUnits s (Vector n (Maybe Unit))
type HOrders s n = HasOrders s (Vector n (Order n))

