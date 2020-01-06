{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Board     (Board, emptyBoard)
import Interface (loop)

import Data.Proxy      (Proxy)
import GHC.TypeNats    (SomeNat (..), someNatVal)
import Numeric.Natural (Natural)
import System.IO       (hGetLine, stdin)
import Text.Read       (readMaybe)

getNat :: IO Natural
getNat = do
  input <- hGetLine stdin -- Not getLine to avoid input termination weirdness.
  let parsed = fromInteger <$> readMaybe input
  case parsed of
    Just x -> return x
    Nothing -> do
      putStrLn "Invalid input, defaulting to zero."
      return 0

run :: SomeNat -> IO ()
run x = case x of SomeNat (_ :: Proxy n) -> loop (emptyBoard :: Board n)

main :: IO ()
main = do
  putStrLn "Board size?"
  nat <- getNat
  run $ someNatVal nat

