module Main where

import Board       (Board)
import Engine.Test
import Interface

import qualified Data.Text.IO as T

main :: IO Board
main = loop smallBoard

