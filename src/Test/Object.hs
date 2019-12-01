{-# LANGUAGE OverloadedStrings #-}

module Test.Object where

import Board.Sandpile
import Board.StaticUnit
import Core.Type
import Slime.Resolve
import Unit.Resolve

import           Control.Monad.State (evalState)
import qualified Data.HashMap.Lazy   as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set

test1Nodes :: HashSet Node
test1Nodes = Set.fromList
  [ "Apple"
  , "Blueberry"
  , "Cherry"
  , "Durian"
  , "Fig" ]

test1Slime :: NodeAttr Slime
test1Slime = HM.fromList
  [ ("Apple", 7)
  , ("Blueberry", 3)
  , ("Cherry", 2)
  , ("Fig", 17)
  , ("Grape", 0) ]

test1Edges :: NodeAttr [Node]
test1Edges = HM.fromList
  [ ("Apple", ["Blueberry"])
  , ("Blueberry", ["Fig"])
  , ("Elderberry", ["Cherry"])
  , ("Cherry", [ "Cherry"
               , "Elderberry" ])
  , ("Fig", [ "Apple"
            , "Apple"
            , "Blueberry"
            , "Cherry" ]) ]

test1Sandpile :: Sandpile
test1Sandpile = evalState (makeSandpile test1Edges test1Slime) test1Nodes

test1Resolve :: NodeAttr Slime
test1Resolve = evalState resolveSlime test1Sandpile

test2Nodes :: HashSet Node
test2Nodes = Set.fromList
  [ "Hazelnut"
  , "Iceberg Lettuce"
  , "Jackfruit"
  , "Kiwi"
  , "Lemon"
  , "Mango"
  , "Nectarine"
  , "Orange"
  , "Peach"
  , "Quince"
  , "Rutabaga"
  , "Squash"
  , "Turnip"
  , "Ugli Fruit"
  , "Vanilla"
  , "Watermellon"
  , "Xigua"
  , "Yam"
  , "Zucchini" ]

test2Edges :: NodeAttr [Node]
test2Edges = HM.fromList
  [ ( "Hazelnut",
    [ "Iceberg Lettuce"
    , "Lemon" ]
    )
  , ( "Iceberg Lettuce",
    [ "Hazelnut"
    , "Mango"
    , "Jackfruit" ]
    )
  , ( "Jackfruit",
    [ "Iceberg Lettuce"
    , "Nectarine"
    , "Kiwi" ]
    )
  , ( "Kiwi",
    [ "Jackfruit"
    , "Orange" ]
    )
  , ( "Lemon",
    [ "Hazelnut"
    , "Mango"
    , "Peach" ]
    )
  , ( "Mango",
    [ "Lemon"
    , "Iceberg Lettuce"
    , "Nectarine"
    , "Quince" ]
    )
  , ( "Nectarine",
    [ "Mango"
    , "Jackfruit"
    , "Orange"
    , "Rutabaga" ]
    )
  , ( "Orange",
    [ "Nectarine"
    , "Kiwi"
    , "Squash" ]
    )
  , ( "Peach",
    [ "Lemon"
    , "Quince"
    , "Turnip" ]
    )
  , ( "Quince",
    [ "Peach"
    , "Mango"
    , "Rutabaga"
    , "Ugli Fruit" ]
    )
  , ( "Rutabaga",
    [ "Quince"
    , "Nectarine"
    , "Squash"
    , "Vanilla" ]
    )
  , ( "Squash",
    [ "Rutabaga"
    , "Orange"
    , "Watermellon" ]
    )
  , ( "Turnip",
    [ "Peach"
    , "Ugli Fruit" ]
    )
  , ( "Ugli Fruit",
    [ "Turnip"
    , "Quince"
    , "Vanilla" ]
    )
  , ( "Vanilla",
    [ "Ugli Fruit"
    , "Rutabaga"
    , "Watermellon" ]
    )
  , ( "Watermellon",
    [ "Squash" ]
    )
  , ( "Yam",
    [ "Xigua" ]
    )
  ]

test2Slime :: NodeAttr Slime
test2Slime = HM.fromList
  [ ("Hazelnut", 2)
  , ("Iceberg Lettuce", 1)
  , ("Jackfruit", 3)
  , ("Kiwi", 1)
  , ("Mango", 1)
  , ("Nectarine", 0)
  , ("Orange", -1)
  , ("Peach", 7)
  , ("Quince", 2)
  , ("Rutabaga", 3)
  , ("Squash", 6)
  , ("Turnip", 2)
  , ("Ugli Fruit", 7)
  , ("Vanilla", 2)
  , ("Watermellon", 3)
  , ("Xigua", 2)
  , ("Yam", 6)
  , ("Zucchini", 1) ]

test2Units :: NodeAttr (Maybe Unit)
test2Units = HM.fromList
  [ ("Dog food", Just Lobber)
  , ("Hazelnut", Just Sprayer)
  , ("Kiwi", Just Sprayer)
  , ("Nectarine", Just Sprayer)
  , ("Rutabaga", Nothing)
  , ("Squash", Just Lobber)
  , ("Turnip", Just Lobber)
  , ("Watermellon", Just Sprayer)
  , ("Xigua", Just Lobber)
  , ("Zucchini", Just Sprayer) ]

test2StaticUnitA :: StaticUnit
test2StaticUnitA = evalState
  (makeStaticUnit test2Edges test2Slime test2Units) test2Nodes

test2ResolveA :: NodeAttr Slime
test2ResolveA = evalState applySprayer test2StaticUnitA

test2StaticUnitB :: StaticUnit
test2StaticUnitB = evalState
  (makeStaticUnit test2Edges test2ResolveA test2Units) test2Nodes

test2ResolveB :: NodeAttr Slime
test2ResolveB = evalState applyLobber test2StaticUnitB

