{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Instances where

import Board
import Engine

import Data.Fin               (Fin (..), universe)
import Data.Proxy             (Proxy (..))
import Data.Type.Nat          (Nat (..), SNatI (..), reflectToNum)
import Data.Vec.Lazy          (Vec (..))
import Test.SmallCheck.Series

instance Monad m => Serial m Nat where
  series = cons0 Z \/ cons1 S

instance (Monad m, SNatI n) => Serial m (Fin n) where
  series = generate $ \d -> take d universe

instance (SNatI n, Serial m a) => Serial m (Vec n a) where
  series = sequenceA $ pure series

instance Monad m => Serial m Slime where
  series = newtypeCons Slime
instance Monad m => Serial m Unit
instance (Monad m, SNatI n) => Serial m (Order n) where
  series = newtypeCons Order
instance (Monad m, SNatI n) => Serial m (OneOrder n) where
  series = localDepth (+1) $ cons2 OneOrder

instance (Monad m , SNatI n) => Serial m (Board n) where
  series = cons4 makeBoard

newtype ShallowBoard m (n :: Nat) = ShallowBoard {_shallow :: Board m}
  deriving Show

-- Maybe worth testing that don't hang: (3,3,2), (2,4,2), (4,2,2)
-- length (listSeries 3 :: [SmallBoard Nat3 Nat2]) = 221184
-- length (listSeries 2 :: [SmallBoard Nat4 Nat2]) =  65536
-- length (listSeries 4 :: [SmallBoard Nat2 Nat2]) =  11664
instance (Monad f, SNatI m, SNatI n) => Serial f (ShallowBoard m n) where
  series = do
    edges <- localDepth (min (reflectToNum (Proxy :: Proxy n))) series
    ShallowBoard <$> cons3 (makeBoard edges)

