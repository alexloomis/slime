{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import Board
import Board.Printer
import Engine
import Engine.Test
import Engine.Util.MaybeNat
import Test.Gen             ()

import qualified Data.HashMap.Lazy as HM
import           Data.List         (sort)
import           Test.Framework

main :: IO ()
main = htfMain htf_thisModulesTests

test_update_slime :: IO ()
test_update_slime =
  assertEqual
  (fmap (\(_,Slime x) -> x) . sort . HM.toList . getSlime $ test1Resolve)
  [MNat 8, MNat 11, MNat 6, MNat 0, MNat 4]

test_unit_action :: IO ()
test_unit_action =
  assertEqual
  (fmap (\(_,Slime x) -> x) . sort . HM.toList . getSlime $ test2Resolve)
  [ MNat 2, MNat 0, MNat 1, MNat 1
  , MNat 0, MNat 0, MNat 0, MErr
  , MNat 0, MNat 2, MNat 2, MNat 5
  , MNat 2, MNat 0, MNat 2, MNat 0
  , MNat 2, MNat 6, MNat 1]

test_unit_move :: IO ()
test_unit_move =
  assertEqual
  (sort . HM.toList . getUnits $ test3Resolve)
  [("Apricot",Nothing)
  ,("Banana",Just Lobber)
  ,("Cabbage",Just Lobber)
  ,("Dates",Just Sprayer)
  ,("Eggplant",Nothing)
  ,("Falafel",Just Sprayer)
  ,("Garlic",Just Sprayer)
  ,("Halibut",Nothing)
  ,("Ice Cream",Just Lobber)
  ,("Jalapeno",Just Sprayer)]

-- Prop tests for exported functions
-- giveOrder

nodeEquality :: HNodes s => (s -> NodeAttr a) -> s -> Bool
nodeEquality f s = getNodes s == HM.keysSet (f s)

prop_arbitrary_board_node_eq_edges :: Board -> Bool
prop_arbitrary_board_node_eq_edges = nodeEquality getEdges

prop_arbitrary_board_node_eq_slime :: Board -> Bool
prop_arbitrary_board_node_eq_slime = nodeEquality getSlime

prop_arbitrary_board_node_eq_units :: Board -> Bool
prop_arbitrary_board_node_eq_units = nodeEquality getUnits

prop_arbitrary_board_node_eq_orders :: Board -> Bool
prop_arbitrary_board_node_eq_orders = nodeEquality getOrders

makeableAfter :: (Board -> Board) -> Board -> Property
makeableAfter res b = classify (b == res b) "trivial" $
  res b === uncurry5 makeBoard (boardAttrs $ res b)
  where uncurry5 f (u,v,w,x,y) = f u v w x y

prop_makeable_after_resolve :: Board -> Property
prop_makeable_after_resolve b = whenFail' (printBoard b >>= const (print b))
  . conjoin $
  [ makeableAfter id b
  , makeableAfter resolveSlime b
  , makeableAfter resolveOrders b
  , makeableAfter resolveUnits b
  , makeableAfter resolveDeaths b ]

prop_board_makeable_after_resolve_slime :: Board -> Property
prop_board_makeable_after_resolve_slime = makeableAfter resolveSlime

prop_board_makeable_after_resolve_orders :: Board -> Property
prop_board_makeable_after_resolve_orders = makeableAfter resolveOrders

prop_board_makeable_after_resolve_units :: Board -> Property
prop_board_makeable_after_resolve_units = makeableAfter resolveUnits

-- Fails on: replayStrings 1 2 3 4
prop_board_makeable_after_resolve_deaths :: Board -> Property
prop_board_makeable_after_resolve_deaths = withMaxSuccess 1000
 . makeableAfter resolveDeaths

-- replayString1 = "Just (SMGen 16607647736078848063 5859287728921095707,38)" :: String
-- replayString2 = "Just (SMGen 1150563981264410373 7885607369095724133,83)" :: String
-- replayString3 = "Just (SMGen 17252415138407314714 5225726372301396331,59)" :: String
-- replayString4 = "Just (SMGen 2061801798427438348 12612330272607078813,98)" :: String

{-
prop_board_makeable_after_resolve_deaths_replay_verbose_1
  :: WithQCArgs (Board -> Property)
prop_board_makeable_after_resolve_deaths_replay_verbose_1 =
  withQCArgs (\a -> a { replay = read replayString1 })
  (verboseShrinking . propertyTest)

propertyTest :: Board -> Property
propertyTest b = ioProperty $ do
  printBoard b
  print b
  return . makeableAfter resolveDeaths $ b
-}

preserveNodes :: HNodes s => (s -> s) -> s -> Bool
preserveNodes f s = getNodes s == getNodes (f s)

prop_pack_attr_preserve_nodes :: NodeAttr [Node] -> Board -> Bool
prop_pack_attr_preserve_nodes e = preserveNodes (packAttr e)


