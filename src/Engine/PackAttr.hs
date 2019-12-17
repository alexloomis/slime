{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
module Engine.PackAttr
  ( PackAttr (..)
  ) where

import Engine.Internal.Type
import Engine.Internal.Util
import Engine.Unit.Order

import           Control.Lens.Combinators
import           Control.Monad.State
import           Data.Default.Class       (Default (..))
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as Set

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

