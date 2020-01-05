{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Board     (Board, emptyBoard)
import Interface (loop)

import Data.Singletons (Sing, SomeSing (..), toSing)
import Data.Type.Nat   (Nat (..), SNat (..), SNatI)
import System.IO       (hGetLine, stdin)
import Text.Read       (readMaybe)

getNat :: IO Nat
getNat = do
  input <- hGetLine stdin -- Not getLine to avoid input termination weirdness.
  let parsed = fromInteger <$> readMaybe input :: Maybe Nat
  case parsed of
    Just x -> return x
    Nothing -> do
      putStrLn "Invalid input, defaulting to zero."
      return 0

-- main :: IO (Board _)
main = do
  putStrLn "Board size?"
  n <- getNat
  return undefined
  {-
  case toSing n of
    -- SomeSing (_ :: Sing n) -> do
      -- loop (emptyBoard :: Board n)
    _ -> undefined
-}

