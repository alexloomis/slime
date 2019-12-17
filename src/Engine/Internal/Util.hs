{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Internal.Util where

import Engine.Internal.Type

import           Control.Lens.Combinators
import           Data.Default.Class       (Default (..))
import           Data.Hashable
import           Data.HashMap.Lazy        (HashMap)
import qualified Data.HashMap.Lazy        as HM
import           Data.Maybe               (fromMaybe)

divOrZero :: Integral a => a -> a -> a
divOrZero num = \case
    0 -> 0
    x -> div num x

val :: Getting (NodeAttr v) s (NodeAttr v) -> Node -> s -> Maybe v
val field node s = HM.lookup node $ view field s

valOrDefault :: Default v =>
  Getting (NodeAttr v) s (NodeAttr v) -> Node -> s -> v
valOrDefault g n s = fromMaybe def $ HM.lookup n (view g s)

getOrDefault :: Default v => (s -> NodeAttr v) -> Node -> s -> v
getOrDefault g n s = fromMaybe def $ HM.lookup n (g s)

-- Change values in the second map to values from the first,
-- when they exist, without adding extra keys.
mask :: (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
mask m n = HM.union (HM.intersection m n) n

-- Combine as mask, with a function applied to matching keys.
-- Uses ∪-with f (∩ A B) B, which should equal ∪ (∩-with f A B) B.
maskWith :: (Eq k, Hashable k)
  => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
maskWith f m n = HM.unionWith f (HM.intersection m n) n

