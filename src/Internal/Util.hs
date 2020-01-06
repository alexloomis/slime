{-# LANGUAGE TupleSections #-}

module Internal.Util where

import Internal.Type

import Control.Lens      (Getter, (^.))
import Data.Composition  ((.:))
import Data.Finite       (Finite, finites)
import Data.List         (genericLength, sortOn)
import Data.List.Extra   (maximumOn)
import Data.Maybe        (fromJust)
import Data.Vector.Sized (Vector, fromList, ifoldr, imap, index, toList)
import GHC.TypeNats      (KnownNat)
import Numeric.Natural   (Natural)

viewAt :: Getter s (Vector n c) -> s -> NodeID n -> c
viewAt l s n = (`index` n) . (^. l) $ s

divOrZ :: (Integral a, Integral b, Num c) => a -> b -> c
divOrZ _ 0 = 0
divOrZ x y = fromIntegral $ div (toInteger x) (toInteger y)

modOrN :: (Integral a, Integral b, Num c) => a -> b -> c
modOrN a 0 = fromIntegral a
modOrN a b = fromIntegral $ mod (toInteger a) (toInteger b)

neighbors :: HEnds s n => s -> NodeID n -> [NodeID n]
neighbors = ifoldr f [] .: viewAt ends
  where
    f k v l = case v of
      0 -> l
      _ -> k:l

nthNeighbors :: HEnds s n => Natural -> s -> NodeID n -> [NodeID n]
nthNeighbors 0     _ n = [n]
nthNeighbors m s n     = concatMap (neighbors s) (nthNeighbors (pred m) s n)

maximaOn :: Ord b => (a -> b) -> [a] -> [a]
maximaOn _ [] = []
maximaOn f xs = filter (\x -> f x == f (maximumOn f xs)) xs

keysWhere :: (a -> Bool) -> Vector n a -> [Finite n]
keysWhere p = fmap fst . filter snd . toList . imap (\k v -> (k, p v))

setElem :: Finite n -> a -> Vector n a -> Vector n a
setElem n a = imap f
  where
    f k v
      | k == n = a
      | otherwise = v

count :: Eq a => [a] -> [(a, Natural)]
count xs = [ (a, genericLength . filter (== a) $ xs) | a <- xs ]

toMap :: Vector n a -> [(Finite n, a)]
toMap = toList . imap (,)

unsafeFromMap :: KnownNat n => [(Finite n, a)] -> Vector n a
unsafeFromMap xs = if (fmap fst . sortOn fst $ xs) == finites
  then fromJust . fromList . fmap snd . sortOn fst $ xs
  else error "fromMap: invalid keys"

-- From map with a default.
fromMap :: KnownNat n => a -> [(Finite n, a)] -> Vector n a
fromMap a = unsafeFromMap . foldr addIfNew [] . (++) (fmap (,a) finites)
  where
    addIfNew (n,b) xs = if n `elem` fmap fst xs then xs else (n,b):xs

