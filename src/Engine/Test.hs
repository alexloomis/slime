{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Engine.Test
  ( module Engine.Test
  ) where

import Board
import Board.Partial
import Engine.Internal.Type
import Engine.Slime.Resolve
import Engine.Unit

import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as Set

test1Nodes :: HashSet Node
test1Nodes =
  [ "Apple"
  , "Blueberry"
  , "Cherry"
  , "Durian"
  , "Fig" ]

test1Slime :: NodeAttr Slime
test1Slime =
  [ ("Apple", 7)
  , ("Blueberry", 3)
  , ("Cherry", 2)
  , ("Fig", 17)
  , ("Grape", 0) ]

test1Ends :: NodeAttr Ends
test1Ends =
  [ ("Apple", [("Blueberry", 1)])
  , ("Blueberry", [("Fig", 1)])
  , ("Elderberry", [("Cherry", 1)])
  , ("Cherry", [ ("Cherry", 1)
               , ("Elderberry", 1) ])
  , ("Fig", [ ("Apple", 2)
            , ("Blueberry", 1)
            , ("Cherry", 1) ]) ]

sbUnits :: NodeAttr (Maybe Unit)
sbUnits =
  [ ("Apple", Just Lobber)
  , ("Cherry", Just Sprayer) ]

sbCommands :: NodeAttr (Maybe Node)
sbCommands = [ ("Apple", Just "Blueberry") ]

smallBoard :: Board
smallBoard = makeBoard test1Nodes test1Ends test1Slime sbUnits sbCommands

test1Sandpile :: Sandpile
test1Sandpile = makeSandpile test1Nodes test1Ends test1Slime

test1Resolve :: Sandpile
test1Resolve = resolveSlime test1Sandpile

test2Nodes :: HashSet Node
test2Nodes =
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

test2Ends :: NodeAttr Ends
test2Ends =
  [ ( "Hazelnut",
    [ ("Iceberg Lettuce", 1)
    , ("Lemon", 1) ]
    )
  , ( "Iceberg Lettuce",
    [ ("Hazelnut", 1)
    , ("Mango", 1)
    , ("Jackfruit", 1) ]
    )
  , ( "Jackfruit",
    [ ("Iceberg Lettuce", 1)
    , ("Nectarine", 1)
    , ("Kiwi", 1) ]
    )
  , ( "Kiwi",
    [ ("Jackfruit", 1)
    , ("Orange", 1) ]
    )
  , ( "Lemon",
    [ ("Hazelnut", 1)
    , ("Mango", 1)
    , ("Peach", 1) ]
    )
  , ( "Mango",
    [ ("Lemon", 1)
    , ("Iceberg Lettuce", 1)
    , ("Nectarine", 1)
    , ("Quince", 1) ]
    )
  , ( "Nectarine",
    [ ("Mango", 1)
    , ("Jackfruit", 1)
    , ("Orange", 1)
    , ("Rutabaga", 1) ]
    )
  , ( "Orange",
    [ ("Nectarine", 1)
    , ("Kiwi", 1)
    , ("Squash", 1) ]
    )
  , ( "Peach",
    [ ("Lemon", 1)
    , ("Quince", 1)
    , ("Turnip", 1) ]
    )
  , ( "Quince",
    [ ("Peach", 1)
    , ("Mango", 1)
    , ("Rutabaga", 1)
    , ("Ugli Fruit", 1) ]
    )
  , ( "Rutabaga",
    [ ("Quince", 1)
    , ("Nectarine", 1)
    , ("Squash", 1)
    , ("Vanilla", 1) ]
    )
  , ( "Squash",
    [ ("Rutabaga", 1)
    , ("Orange", 1)
    , ("Watermellon", 1) ]
    )
  , ( "Turnip",
    [ ("Peach", 1)
    , ("Ugli Fruit", 1) ]
    )
  , ( "Ugli Fruit",
    [ ("Turnip", 1)
    , ("Quince", 1)
    , ("Vanilla", 1) ]
    )
  , ( "Vanilla",
    [ ("Ugli Fruit", 1)
    , ("Rutabaga", 1)
    , ("Watermellon", 1) ]
    )
  , ( "Watermellon",
    [ ("Squash", 1) ]
    )
  , ( "Yam",
    [ ("Xigua", 1) ]
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
test2Units =
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
test2StaticUnit = makeStaticUnit test2Nodes test2Ends test2Slime test2Units

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

test3Ends :: NodeAttr Ends
test3Ends = HM.fromList
  [ ("Apricot", [("Dates", 1)])
  , ("Cabbage", [("Falafel", 1)])
  , ("Eggplant", [("Banana", 1)])
  , ("Falafel", [("Cabbage", 1)])
  , ("Garlic", [("Ice Cream", 1)])
  , ("Halibut", [("Ice Cream", 1)]) ]

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
test3Position = makeMoveUnit test3Nodes test3Ends test3Units test3Orders

test3Resolve :: MoveUnit
test3Resolve = resolveOrders test3Position

