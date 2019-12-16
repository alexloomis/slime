{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
module Engine.PackAttr where

import Engine.Internal.Type
import Engine.Unit.Order

import           Control.Lens.Combinators
import           Control.Monad.State
import           Data.Default.Class       (Default (..))
import           Data.Hashable            (Hashable)
import           Data.HashMap.Lazy        (HashMap)
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as Set

-- Change values in the second map to values from the first,
-- when they exist, without adding extra keys.
mask :: (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
mask m n = HM.union (HM.intersection m n) n

-- Combine as mask, with a function applied to matching keys.
-- Uses ∪-with f (∩ A B) B, which should equal ∪ (∩-with f A B) B.
maskWith :: (Eq k, Hashable k)
  => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
maskWith f m n = HM.unionWith f (HM.intersection m n) n

allDefault :: (HNodes s, Default v) => s -> NodeAttr v
allDefault = HM.map (const def) . Set.toMap . view nodes

filterEnds :: HNodes s => s -> Ends -> Ends
filterEnds s = HM.fromList . filter predicate . HM.toList
  where
    predicate (n,x) = x >= 0
      && n `Set.member` view nodes s

prePackAttr :: (Default v, HNodes s) => NodeAttr v -> s -> NodeAttr v
prePackAttr a = mask a . allDefault

foldOrders :: (HNodes s, HEnds s, HUnits s, HOrders s)
  => NodeAttr (Maybe Node) -> s -> s
foldOrders a s = foldl (flip giveOrder) s (attrToOrders a)

class PackAttr s a where
  unsafePackAttr :: NodeAttr a -> s -> s
  packAttr :: NodeAttr a -> s -> s
  default packAttr :: (Default a, HNodes s) => NodeAttr a -> s -> s
  packAttr a = liftM2 unsafePackAttr (prePackAttr a) id

instance (HNodes s, HEnds s) => PackAttr s Ends where
  unsafePackAttr = set ends
  packAttr a s = unsafePackAttr
    (maskWith (const . filterEnds s) a $ allDefault s) s
instance (HNodes s, HSlime s) => PackAttr s Slime where
  unsafePackAttr = set slime
instance (HNodes s, HUnits s) => PackAttr s (Maybe Unit) where
  unsafePackAttr = set units
instance (HNodes s, HEnds s, HUnits s, HOrders s)
  => PackAttr s (Maybe Node) where
  unsafePackAttr = set orders
  packAttr a s = foldOrders a s'
    where s' = set orders (allDefault s) s

