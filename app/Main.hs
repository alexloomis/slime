module Main where

import Board     (Board, emptyBoard)
import Interface (loop)

import Data.Type.Nat (Nat5, SNatI)
import System.IO     (hGetLine, stdin)

-- boardSize = Nat5

main :: IO (Board Nat5)
main = do
  putStrLn "Board size?"
  _ <- hGetLine stdin
  putStrLn "Ignoring input, chosing 5."
  loop emptyBoard

