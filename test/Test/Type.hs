{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Test.Type where

import Board
import Engine
import Test.Instances ()

import Control.Lens           (makeFieldsNoPrefix)
import Data.Proxy             (Proxy (..))
import Data.Vector.Sized      (Vector)
import GHC.Generics           (Generic)
import GHC.TypeNats           (KnownNat, Nat, natVal)
import Test.SmallCheck.Series

data BoardES n = BoardES
  { _ends  :: Vector n (Ends n)
  , _slime :: Vector n Slime }
  deriving (Eq, Generic, Show)

data BoardESU n = BoardESU
  { _ends  :: Vector n (Ends n)
  , _slime :: Vector n Slime
  , _units :: Vector n (Maybe Unit) }
  deriving (Eq, Generic, Show)

data BoardEUO n = BoardEUO
  { _ends   :: Vector n (Ends n)
  , _units  :: Vector n (Maybe Unit)
  , _orders :: Vector n (Order n) }
  deriving (Eq, Generic, Show)

data BoardSU n = BoardSU
  { _slime :: Vector n Slime
  , _units :: Vector n (Maybe Unit) }
  deriving (Eq, Generic, Show)

$(makeFieldsNoPrefix ''BoardES)
$(makeFieldsNoPrefix ''BoardESU)
$(makeFieldsNoPrefix ''BoardEUO)
$(makeFieldsNoPrefix ''BoardSU)

instance (Monad m, KnownNat n) => Serial m (BoardES n)

instance (Monad m, KnownNat n) => Serial m (BoardESU n) where
  series = do
    e <- series
    s <- series
    u <- localDepth (+1) series
    return $ BoardESU e s u

instance (Monad m, KnownNat n) => Serial m (BoardEUO n) where
  series = do
    e <- series
    u <- localDepth (+1) series
    o <- series
    return $ BoardEUO e u o

instance (Monad m, KnownNat n) => Serial m (BoardSU n) where
  series = do
    s <- series
    u <- localDepth (+1) series
    return $ BoardSU s u

newtype Shallow m (n :: Nat) = Shallow {_shallow :: Board m} deriving Show

-- Maybe worth testing that don't hang:
-- length (listSeries 4 :: [Shallow 2 2]) = 242757
-- length (listSeries 2 :: [Shallow 3 1]) = 221184
instance (Monad f, KnownNat m, KnownNat n) => Serial f (Shallow m n) where
  series = do
    depth <- getDepth
    e <- localDepth (min (fromEnum $ natVal (Proxy :: Proxy n))) series
    s <- localDepth (min (fromEnum $ natVal (Proxy :: Proxy n))) series
    u <- localDepth (const depth) series
    o <- localDepth (const depth) series
    Shallow <$> cons0 (makeBoard e s u o)

