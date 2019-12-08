{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Internal.Util where

import Engine.Internal.Type

import           Control.Lens.Combinators
import           Data.Default.Class       (Default (..))
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

