module Internal.Slime where

import Internal.Import

import           Control.Monad    (liftM2)
import           Data.Composition ((.:))
import qualified Data.Vec.Lazy    as Vec

numEndsFrom :: HEnds s n => s -> NodeID n -> Slime
numEndsFrom = Slime . Vec.sum .: viewAt ends

slimePerEnd :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
slimePerEnd = (liftM2 . liftM2) divOrZ (viewAt slime) numEndsFrom

{-
remainingSlime :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
remainingSlime = (liftM3 . liftM3)
  (\a b c -> a - b * c) (viewAt slime) slimePerEnd numEndsFrom
-}

remainingSlime :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
remainingSlime = (liftM2 . liftM2) modOrN (viewAt slime) numEndsFrom

incomingEnds :: HEnds s n => s -> NodeID n -> Ends n
incomingEnds s n = fmap (! n) $ s ^. ends

addedSlime :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
addedSlime s = Vec.sum . imap (\k v -> slimePerEnd s k * Slime v) . incomingEnds s

newSlime :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
newSlime = (liftM2 . liftM2) (+) addedSlime remainingSlime

resolveSlime :: (HEnds s n, HSlime s n, SNatI n) => s -> s
resolveSlime s = set slime (Vec.tabulate . newSlime $ s) s

