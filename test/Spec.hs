{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Board
import Engine
import GameState
import Test.Instances

import Control.Monad         (liftM2, liftM4)
import Data.List             (sort)
import Data.Proxy            (Proxy (..))
import Data.Type.Nat
import Data.Vec.Lazy         (toList)
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Text.Megaparsec       (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [scProps]

scProps :: TestTree
scProps = testGroup "SmallCheck" $
  fmap (localOption (3 :: SmallCheckDepth)
  . ($ (Proxy :: Proxy (Shallow Nat3 Nat2))))
  [makeableAfter, fieldInvariants, parseId]

---------- makeableAfter
remakeBoard :: Board n -> Board n
remakeBoard = liftM4 makeBoard _ends _slime _units (toOneOrders . _orders)

makeAfter :: forall m n. (SNatI m, SNatI n) =>
  Proxy (Shallow m n) -> TestName -> (Board m -> Board m) -> TestTree
makeAfter _ text f = SC.testProperty text $ liftM2 (==)
  (f . __shallow) (remakeBoard . f . __shallow)
  where
    __shallow :: Shallow m n -> Board m
    __shallow = _shallow

makeableAfter :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
makeableAfter p = testGroup "Makeable after"
  [ makeAfter p "resolveSlime" resolveSlime
  , makeAfter p "resolveDeaths" resolveDeaths
  , makeAfter p "resolveUnits" resolveUnits
  , makeAfter p "resolveOrders" resolveOrders ]

---------- Parse id
piTest :: SNatI m => Proxy (Shallow m n) -> Shallow m n -> Bool
piTest _ = liftM2 (==) (parse parseGame "" . showGame . _shallow) (Right . _shallow)

parseId :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
parseId = SC.testProperty "Parse id" . piTest

---------- Field invariant
fieldId :: forall m a n . (SNatI m, SNatI n, Eq a) => (Board m -> a)
  -> Proxy (Shallow m n) -> TestName -> (Board m -> Board m) -> TestTree
fieldId g _ t f = SC.testProperty t $
  liftM2 (==) (g . __shallow) (g . f . __shallow)
  where
    __shallow :: Shallow m n -> Board m
    __shallow = _shallow

endId :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
endId p = testGroup "Ends identical"
  [ fieldId getEnds p "resolveSlime" resolveSlime
  , fieldId getEnds p "resolveUnits" resolveUnits
  , fieldId getEnds p "resolveOrders" resolveOrders
  , fieldId getEnds p "resolveDeaths" resolveDeaths ]

slimeId :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
slimeId p = testGroup "Slime identical"
  [ fieldId getSlime p "resolveDeaths" resolveDeaths
  , fieldId getSlime p "resolveOrders" resolveOrders ]

unitId :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
unitId p = testGroup "Units identical"
  [ fieldId getUnits p "resolveSlime" resolveSlime
  , fieldId getUnits p "resolveUnits" resolveUnits ]

orderId :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
orderId p = testGroup "Orders identical"
  [ fieldId getOrders p "resolveSlime" resolveSlime
  , fieldId getOrders p "resolveUnits" resolveUnits ]

slimeCons :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
slimeCons p = fieldId (sum . getSlime) p
  "Slime conserved after resolve" resolveSlime

unitCons :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
unitCons p = fieldId (sort . toList . getUnits) p
  "Units conserved after move" resolveOrders

fieldInvariants :: (SNatI m, SNatI n) => Proxy (Shallow m n) -> TestTree
fieldInvariants p = testGroup "Field invariants"
  [ endId p
  , slimeId p
  , unitId p
  , orderId p
  , slimeCons p
  , unitCons p ]

