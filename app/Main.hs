module Main where

import Board     (Board, emptyBoard)
import Interface (loop)

main :: IO Board
main = putStrLn "Ready." >> loop emptyBoard

