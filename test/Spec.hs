{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
import Board
import Engine
import Test.Instances

import Control.Monad          (liftM2, liftM4)
import Data.Type.Nat
import Test.SmallCheck.Series (Serial)
import Test.Tasty
import Test.Tasty.SmallCheck  as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [scProps]

scProps :: TestTree
scProps = testGroup "SmallCheck" [makeableAfter, slimeConserved]

remakeBoard :: Board n -> Board n
remakeBoard = liftM4 makeBoard _ends _slime _units (toOneOrders . _orders)

makeAfter :: (Serial IO a, Show a) => TestName -> (a -> Board n) -> TestTree
makeAfter text f = SC.testProperty text $ liftM2 (==) f (remakeBoard . f)

makeableAfter :: TestTree
makeableAfter = localOption d $ testGroup "Makeable after"
  [ makeAfter "resolveSlime"
    (resolveSlime . _shallow :: ShallowBoard Nat3 Nat2 -> _)
  , makeAfter "resolveDeaths"
    (resolveDeaths . _shallow :: ShallowBoard Nat3 Nat2 -> _)
  , makeAfter "resolveUnits"
    (resolveUnits . _shallow :: ShallowBoard Nat3 Nat2 -> _)
  , makeAfter "resolveOrders"
    (resolveOrders . _shallow :: ShallowBoard Nat3 Nat2 -> _) ]
  where d = 3 :: SmallCheckDepth

scTest :: SNatI m => ShallowBoard m n -> Bool
scTest = liftM2 (==) (sum . getSlime . _shallow)
    (sum . getSlime . resolveSlime . _shallow)

slimeConserved :: TestTree
slimeConserved = localOption d $ SC.testProperty "Slime conserved"
  (scTest :: ShallowBoard Nat3 Nat2 -> _)
  where d = 3 :: SmallCheckDepth

