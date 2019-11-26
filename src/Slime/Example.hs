{-# LANGUAGE OverloadedStrings #-}

module Slime.Example where

import Core.Type
import Position
import Slime.Resolve
import Slime.Type

import Control.Monad.State (evalState)
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as HM

testNodes :: HashSet Node
testNodes =
  Set.fromList
    [ "Apple"
    , "Blueberry"
    , "Cherry"
    , "Fig"
    ]

testSlime :: Slime
testSlime = HM.fromList
  [ ("Apple", 7)
  , ("Blueberry", 3)
  , ("Cherry", 2)
  , ("Fig", 17)
  , ("Grape", 0) ]

testEdges :: [(Node, Node)]
testEdges =
  [ ("Apple", "Blueberry")
  , ("Blueberry", "Fig")
  , ("Cherry", "Cantaloupe")
  , ("Cherry", "Cherry")
  , ("Fig", "Apple")
  , ("Fig", "Apple")
  , ("Fig", "Blueberry")
  , ("Fig", "Cherry")
  ]

testPosition :: Position
testPosition = evalState (makePosition testEdges testSlime) testNodes

testResolve :: Slime
testResolve = evalState resolveSlime testPosition
