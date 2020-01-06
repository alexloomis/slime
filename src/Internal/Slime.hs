module Internal.Slime where

import Internal.Import

import           Control.Monad     (liftM2)
import           Data.Composition  ((.:))
import qualified Data.Vector.Sized as V

numEndsFrom :: HEnds s n => s -> NodeID n -> Slime
numEndsFrom = Slime . V.sum .: viewAt ends

slimePerEnd :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
slimePerEnd = (liftM2 . liftM2) divOrZ (viewAt slime) numEndsFrom

remainingSlime :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
remainingSlime = (liftM2 . liftM2) modOrN (viewAt slime) numEndsFrom

incomingEnds :: HEnds s n => s -> NodeID n -> Ends n
incomingEnds s n = fmap (`index` n) $ s ^. ends

addedSlime :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
addedSlime s = V.sum . imap (\k v -> slimePerEnd s k * Slime v) . incomingEnds s

newSlime :: (HEnds s n, HSlime s n) => s -> NodeID n -> Slime
newSlime = (liftM2 . liftM2) (+) addedSlime remainingSlime

resolveSlime :: (HEnds s n, HSlime s n, KnownNat n) => s -> s
resolveSlime s = set slime (V.generate . newSlime $ s) s

