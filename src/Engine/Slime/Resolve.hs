module Engine.Slime.Resolve
  ( slimePerEnd
  , newSlime
  , resolveSlime
  ) where

import Engine.Internal.Type
import Engine.Internal.Util

import           Control.Lens.Combinators
import           Control.Monad.State
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as HS

-- If node is not valid, returns zero.
numEndsFrom :: HEnds s => Node -> s -> Slime
numEndsFrom node = fromIntegral . sum . HM.elems . valOrDefault ends node

-- If node is not valid, returns zero.
slimePerEnd :: (HEnds s, HSlime s) => Node -> s -> Slime
slimePerEnd = (liftM2 . liftM2) divOrZero (valOrDefault slime) numEndsFrom

-- If node is not valid, returns zero.
remainingMap :: (HEnds s, HSlime s) => Node -> s -> Slime
remainingMap = (liftM3 . liftM3)
  (\a b c -> a - b * c) (valOrDefault slime) slimePerEnd numEndsFrom

remainingSlime :: (HEnds s, HSlime s) => s -> NodeAttr Slime
remainingSlime s = HM.fromList
  . fmap (mapToSnd (`remainingMap` s)) . HM.keys . view ends $ s
    where mapToSnd f a = (a, f a)

incomingEnds :: HEnds s => Node -> s -> Ends
incomingEnds n = HM.mapMaybe (HM.lookup n) . getEnds

addedSlime :: (HNodes s, HEnds s, HSlime s) => s -> NodeAttr Slime
addedSlime s = HM.fromList . fmap f . HS.toList . getNodes $ s
  where
    f n = (n, sum . HM.elems . g $ incomingEnds n s)
    g = HM.mapWithKey (\k v -> slimePerEnd k s * fromIntegral v)

newSlime :: (HNodes s, HEnds s, HSlime s) => s -> NodeAttr Slime
newSlime = liftM2 (HM.unionWith (+)) addedSlime remainingSlime

resolveSlime :: (HNodes s, HEnds s, HSlime s) => s -> s
resolveSlime s = set slime (newSlime s) s

