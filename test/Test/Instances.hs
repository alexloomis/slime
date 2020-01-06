{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Instances where

import Board
import Engine

import Data.Finite            (Finite, finites)
import Data.Vector.Sized      (Vector)
import GHC.TypeNats           (KnownNat)
import Test.SmallCheck.Series

instance (Monad m, KnownNat n) => Serial m (Finite n) where
  series = generate $ \d -> take d finites

instance (KnownNat n, Serial m a) => Serial m (Vector n a) where
  series = sequenceA $ pure series

instance Monad m => Serial m Slime where
  series = newtypeCons Slime
instance Monad m => Serial m Unit
instance (Monad m, KnownNat n) => Serial m (Order n) where
  series = newtypeCons Order
instance (Monad m, KnownNat n) => Serial m (OneOrder n) where
  series = localDepth (+1) $ cons2 OneOrder

instance (Monad m , KnownNat n) => Serial m (Board n) where
  series = cons4 makeBoard

