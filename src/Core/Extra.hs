module Core.Extra where

import Control.Monad.State

stuffState :: (a -> State b c) -> State b (a -> c)
stuffState f = state . mapToFst . flip $ evalState . f
  where
    mapToFst f a = (f a, a)

unstuffState :: State b (a -> c) -> a -> State b c
unstuffState s a = mapState (applyToFst a) s
  where
    applyToFst a (f, b) = (f a, b)

divOrZero :: Integral a => a -> a -> a
divOrZero num denom =
  case denom of
    0 -> 0
    x -> div num x
