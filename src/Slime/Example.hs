{-# LANGUAGE OverloadedStrings #-}

module Slime.Example where

import Core.Type
import Position
import Slime.Resolve
import Slime.Type

import           Control.Monad.State (evalState)
import qualified Data.HashMap.Lazy   as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set

testNodes :: HashSet Node
testNodes =
  Set.fromList
    [ "Apple"
    , "Blueberry"
    , "Cherry"
    , "Durian"
    , "Fig" ]

testSlime :: NodeAttr Slime
testSlime = HM.fromList
  [ ("Apple", 7)
  , ("Blueberry", 3)
  , ("Cherry", 2)
  , ("Fig", 17)
  , ("Grape", 0) ]

testEdges :: NodeAttr [Node]
testEdges = HM.fromList
  [ ("Apple", ["Blueberry"])
  , ("Blueberry", ["Fig"])
  , ("Cantaloupe", ["Cherry"])
  , ("Cherry", ["Cherry"])
  , ("Fig", [ "Apple"
            , "Apple"
            , "Blueberry"
            , "Cherry" ]) ]

testPosition :: Position
testPosition = evalState (makePosition testEdges testSlime) testNodes

testResolve :: NodeAttr Slime
testResolve = evalState resSlime testPosition
