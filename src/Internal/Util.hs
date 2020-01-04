module Internal.Util where

import Internal.Type

import Control.Lens     (Getter, (^.))
import Data.Composition ((.:))
import Data.Fin         (Fin (..), universe)
import Data.List        (genericLength, sortOn)
import Data.List.Extra  (maximumOn)
import Data.Maybe       (fromJust)
import Data.Type.Nat    (Nat (..), SNatI)
import Data.Vec.Lazy    (Vec, fromList, ifoldr, imap, toList, (!))

viewAt :: Getter s (Vec n c) -> s -> NodeID n -> c
viewAt l s n = (! n) . (^. l) $ s

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
      Z -> l
      _ -> k:l

nthNeighbors :: HEnds s n => Nat -> s -> NodeID n -> [NodeID n]
nthNeighbors Z     _ n = [n]
nthNeighbors (S m) s n = concatMap (neighbors s) (nthNeighbors m s n)

maximaOn :: Ord b => (a -> b) -> [a] -> [a]
maximaOn _ [] = []
maximaOn f xs = filter (\x -> f x == f (maximumOn f xs)) xs

keysWhere :: (a -> Bool) -> Vec n a -> [Fin n]
keysWhere p = fmap fst . filter snd . toList . imap (\k v -> (k, p v))

setElem :: Fin n -> a -> Vec n a -> Vec n a
setElem n a = imap f
  where
    f k v
      | k == n = a
      | otherwise = v

count :: Eq a => [a] -> [(a, Nat)]
count xs = [ (a, genericLength . filter (== a) $ xs) | a <- xs ]

toMap :: Vec n a -> [(Fin n, a)]
toMap = toList . imap (,)

-- Unsafe!
fromMap :: SNatI n => [(Fin n, a)] -> Vec n a
fromMap xs = if (fmap fst . sortOn fst $ xs) == universe
  then fromJust . fromList . fmap snd . sortOn fst $ xs
  else error "fromMap: invalid keys"

