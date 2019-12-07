{-# LANGUAGE OverloadedStrings #-}

module Engine.Test
  ( module Engine.Test
  ) where

import Board.MoveUnit
import Board.Sandpile
import Board.StaticUnit
import Engine.Internal.Type
import Engine.Slime.Resolve
import Engine.Unit

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
test1Sandpile = makeSandpile test1Nodes test1Edges test1Slime

test1Resolve :: Sandpile
test1Resolve = resolveSlime test1Sandpile

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

test2StaticUnit :: StaticUnit
test2StaticUnit = makeStaticUnit test2Nodes test2Edges test2Slime test2Units

test2Resolve :: StaticUnit
test2Resolve = resolveUnits test2StaticUnit

test2TurnNoMove :: StaticUnit
test2TurnNoMove = removeUnits . resolveSlime . resolveUnits $ test2StaticUnit

test3Nodes :: HashSet Node
test3Nodes = Set.fromList
  [ "Apricot"
  , "Banana"
  , "Cabbage"
  , "Dates"
  , "Eggplant"
  , "Falafel"
  , "Garlic"
  , "Halibut"
  , "Ice Cream"
  , "Jalapeno" ]

test3Edges :: NodeAttr [Node]
test3Edges = HM.fromList
  [ ("Apricot", ["Dates"])
  , ("Cabbage", ["Falafel"])
  , ("Eggplant", ["Banana"])
  , ("Falafel", ["Cabbage"])
  , ("Garlic", ["Ice Cream"])
  , ("Halibut", ["Ice Cream"]) ]

test3Units :: NodeAttr (Maybe Unit)
test3Units = HM.fromList
  [ ("Apricot", Just Sprayer)
  , ("Banana", Just Lobber)
  , ("Cabbage", Just Sprayer)
  , ("Falafel", Just Lobber)
  , ("Garlic", Just Sprayer)
  , ("Halibut", Just Lobber)
  , ("Jalapeno", Just Sprayer) ]

test3Orders :: NodeAttr (Maybe Node)
test3Orders = HM.fromList
  [ ("Apricot", Just "Dates")
  , ("Banana", Just "Eggplant")
  , ("Cabbage", Just "Falafel")
  , ("Falafel", Just "Cabbage")
  , ("Garlic", Just "Ice Cream")
  , ("Halibut", Just "Ice Cream") ]

test3Position :: MoveUnit
test3Position = makeMoveUnit test3Nodes test3Edges test3Units test3Orders

test3Resolve :: MoveUnit
test3Resolve = resolveOrders test3Position

