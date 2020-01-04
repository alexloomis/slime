module Internal.Unit.Sprayer where

import Internal.Import

import Data.List  (nub)
import Data.Tuple (swap)

sprayerDamage :: Slime
sprayerDamage = 1

sprayerRange :: Nat
sprayerRange = 1

-- Targets should be listed as many times as they are targeted.
sprayerTargets :: (HEnds s n, HUnits s n) => s -> [NodeID n]
sprayerTargets s = concat . toList $ imap onlySprayers (s ^. ends)
  where
    onlySprayers k _ = case viewAt units s k of
      Just Sprayer -> nub $ nthNeighbors sprayerRange s k
      _            -> []

reduceNodeBy :: HSlime s n => Nat -> NodeID n -> s -> s
reduceNodeBy Z _ s = s
reduceNodeBy (S m) n s = case viewAt slime s n of
  Slime Z     -> s
  Slime (S l) -> reduceNodeBy m n $ over slime (setElem n (Slime l)) s

countedTargets :: (HEnds s n, HUnits s n) => s -> [(Nat, NodeID n)]
countedTargets = fmap swap . count . sprayerTargets

resolveSprayer :: (HEnds s n, HSlime s n, HUnits s n) => s -> s
resolveSprayer s = foldr (uncurry reduceNodeBy) s (countedTargets s)

