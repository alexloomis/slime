{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}

module Core.Extra.MaybeNat
  ( MaybeNat (..)
  , fmapOver
  , liftOver
  ) where

import Data.Bits        (Bits (..), popCountDefault)
import Data.Data        (Data (..))
import Data.Dynamic     (Typeable)
import Data.Ix          (Ix (..))
import Data.Maybe       (fromMaybe)
import Data.Tuple.Extra (both)
import GHC.Arr          (indexError)
import GHC.Generics     (Generic)
import Numeric.Natural  (Natural)
import Text.Printf      (PrintfArg (..), formatInteger)

data MaybeNat = MErr | MNat Natural
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

fmapOver :: (Natural -> Natural) -> MaybeNat -> MaybeNat
fmapOver f (MNat n) = MNat $ f n
fmapOver _ _        = MErr

liftOver :: (Natural -> Natural -> Natural) -> MaybeNat -> MaybeNat -> MaybeNat
liftOver f (MNat m) (MNat n) = MNat $ f m n
liftOver _ _ _               = MErr

instance Num MaybeNat where
  (+) = liftOver (+)
  (*) = liftOver (*)
  m - n
    | m >= n = liftOver (-) m n
    | otherwise = MErr
  fromInteger n
    | n >= 0 =  MNat . fromInteger $ n
    | otherwise = MErr
  abs = id
  signum = const 1

instance Real MaybeNat where
  toRational = \case
    MNat m -> toRational m
    MErr   -> -1

instance Enum MaybeNat where
  fromEnum = \case
    MNat m  -> fromIntegral m
    MErr -> -1
  toEnum x
    | x < 0 = MErr
    | otherwise = MNat . fromIntegral $ x

instance Integral MaybeNat where
  toInteger = fromIntegral . fromEnum
  quotRem (MNat m) (MNat n) = both MNat $ quotRem m n
  quotRem _ _               = (MErr, MErr)

instance Bits MaybeNat where
  (.&.) = liftOver (.&.)
  (.|.) = liftOver (.&.)
  xor = liftOver xor
  complement = fmapOver complement
  shift n a = fmapOver (`shift` a) n
  rotate n a = fmapOver (`rotate` a) n
  bitSizeMaybe = \case
    MNat m  -> bitSizeMaybe m
    MErr -> Nothing
  bitSize n = fromMaybe
    (error "Bits MaybeNat: bitSize not defined for input.")
    (bitSizeMaybe n)
  isSigned _ = isSigned (0 :: Natural)
  testBit n a = case n of
    MNat m -> testBit m a
    MErr   -> False
  bit = MNat . bit
  popCount = popCountDefault

instance Ix MaybeNat where
  range (m,n) = case m of
    MErr -> []
    _    -> [m..n]
  index (MNat a, MNat b) (MNat c) = index (a,b) c
  index r s                       = indexError r s "MaybeNat"
  inRange a b = elem b $ range a

instance PrintfArg MaybeNat where
  formatArg = \case
    MErr -> formatArg "MErr"
    a -> formatInteger . toInteger $ a


