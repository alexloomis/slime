module Core.Extra where

import Control.Monad.State

packState :: (a -> State b c) -> State b (a -> c)
packState f = state . mapToFst . flip $ evalState . f
  where
    mapToFst f a = (f a, a)

unpackState :: State b (a -> c) -> a -> State b c
unpackState s a = mapState (applyToFst a) s
  where
    applyToFst a (f, b) = (f a, b)

divOrZero :: Integral a => a -> a -> a
divOrZero num denom =
  case denom of
    0 -> 0
    x -> div num x

satSub :: (Num a, Ord a) => a -> a -> a
satSub x y = max (x-y) 0
