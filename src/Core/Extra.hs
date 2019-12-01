{-# LANGUAGE LambdaCase #-}

module Core.Extra where

import Control.Monad.State

packState :: (a -> State b c) -> State b (a -> c)
packState f = state . mapToFst . flip $ evalState . f
  where mapToFst g a = (g a, a)

unpackState :: State b (a -> c) -> a -> State b c
unpackState s a = mapState (applyToFst a) s
  where applyToFst c (f, b) = (f c, b)

divOrZero :: Integral a => a -> a -> a
divOrZero num = \case
    0 -> 0
    x -> div num x

