{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Board
import Engine
import GameState
import Test.Type

import Control.Lens           (view)
import Control.Monad          (liftM2, liftM4)
import Data.List              (sort)
import Data.Proxy             (Proxy (..))
import Data.Vector.Sized      (toList)
import GHC.TypeNats
import Test.SmallCheck.Series (Serial)
import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Text.Megaparsec        (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [scProps]

shallow421 :: (Proxy (Shallow 2 1) -> TestTree) -> TestTree
shallow421 = localOption (4 :: SmallCheckDepth) . ($ (Proxy :: Proxy (Shallow 2 1)))

shallow222 :: (Proxy (Shallow 2 2) -> TestTree) -> TestTree
shallow222 = localOption (3 :: SmallCheckDepth) . ($ (Proxy :: Proxy (Shallow 2 2)))

es23 :: (Proxy (BoardES 3) -> TestTree) -> TestTree
es23 = localOption (2 :: SmallCheckDepth) . ($ (Proxy :: Proxy (BoardES 3)))

esu23 :: (Proxy (BoardESU 3) -> TestTree) -> TestTree
esu23 = localOption (1 :: SmallCheckDepth) . ($ (Proxy :: Proxy (BoardESU 3)))

su43 :: (Proxy (BoardSU 3) -> TestTree) -> TestTree
su43 = localOption (4 :: SmallCheckDepth) . ($ (Proxy :: Proxy (BoardSU 3)))

euo23 :: (Proxy (BoardEUO 3) -> TestTree) -> TestTree
euo23 = localOption (2 :: SmallCheckDepth) . ($ (Proxy :: Proxy (BoardEUO 3)))

scProps :: TestTree
scProps = testGroup "SmallCheck"
  $  fmap shallow421 [parseId]
  ++ fmap shallow222 [makeableAfter]
  ++ fmap es23 [testES]
  ++ fmap esu23 [testESU]
  ++ fmap su43 [testSU]
  ++ fmap euo23 [testEUO]

---------- makeableAfter
remakeBoard :: Board n -> Board n
remakeBoard = liftM4 makeBoard (view ends) (view slime)
  (view units) (toOneOrders . view orders)

makeAfter :: forall m n. (KnownNat m, KnownNat n) =>
  Proxy (Shallow m n) -> TestName -> (Board m -> Board m) -> TestTree
makeAfter _ text f = SC.testProperty text $ liftM2 (==)
  (f . __shallow) (remakeBoard . f . __shallow)
  where
    __shallow :: Shallow m n -> Board m
    __shallow = _shallow

makeableAfter :: (KnownNat m, KnownNat n) => Proxy (Shallow m n) -> TestTree
makeableAfter p = testGroup "Makeable after"
  [ makeAfter p "resolveSlime" resolveSlime
  , makeAfter p "resolveDeaths" (cleanOrders . resolveDeaths)
  , makeAfter p "resolveUnits" resolveUnits
  , makeAfter p "resolveOrders" resolveOrders ]

---------- Parse id
piTest :: KnownNat m => Proxy (Shallow m n) -> Shallow m n -> Bool
piTest _ = liftM2 (==) (parse parseGame "" . showGame . _shallow) (Right . _shallow)

parseId :: (KnownNat m, KnownNat n) => Proxy (Shallow m n) -> TestTree
parseId = SC.testProperty "Parse id" . piTest

---------- Field invariant
fieldId :: (Eq b, Serial IO a, Show a)
  => TestName -> (a -> b) -> (a -> a) -> Proxy a -> TestTree
fieldId t g f _ = SC.testProperty t $ liftM2 (==) g (g . f)

---------- Triviality test
-- nonTrivial :: forall s n. (HSlime s n, KnownNat n) => TestName -> TestTree
-- nonTrivial t = SC.testProperty t . exists $

trivEnds :: (HEnds s n, KnownNat n) => Proxy s -> s -> Bool
trivEnds _ x = getEnds x == getEnds emptyBoard

trivSlime :: (HSlime s n, KnownNat n) => Proxy s -> s -> Bool
trivSlime _ x = getSlime x == getSlime emptyBoard

trivUnits :: (HUnits s n, KnownNat n) => Proxy s -> s -> Bool
trivUnits _ x = getUnits x == getUnits emptyBoard

trivOrders :: (HOrders s n, KnownNat n) => Proxy s -> s -> Bool
trivOrders _ x = getOrders x == getOrders emptyBoard

nonTrivEnds :: (Serial IO s, Show s, HEnds s n, KnownNat n)
  => Proxy s -> TestTree
nonTrivEnds p = SC.testProperty "Nontrivial ends" . exists $ not . trivEnds p

nonTrivSlime :: (Serial IO s, Show s, HSlime s n, KnownNat n)
  => Proxy s -> TestTree
nonTrivSlime p = SC.testProperty "Nontrivial slime" . exists $ not . trivSlime p

nonTrivUnits :: (Serial IO s, Show s, HUnits s n, KnownNat n)
  => Proxy s -> TestTree
nonTrivUnits p = SC.testProperty "Nontrivial units" . exists $ not . trivUnits p

nonTrivOrders :: (Serial IO s, Show s, HOrders s n, KnownNat n)
  => Proxy s -> TestTree
nonTrivOrders p = SC.testProperty "Nontrivial orders" . exists $ not . trivOrders p

-- Type-specific tests
testES :: KnownNat n => Proxy (BoardES n) -> TestTree
testES p = testGroup "BoardES" $
  [ nonTrivEnds p
  , nonTrivSlime p ]
  ++ fmap (after AllSucceed "BoardES.Nontrivial")
  [ fieldId "Ends identical after resolveSlime" getEnds resolveSlime p
  , fieldId "Slime conserved after resolveSlime"
    (sum . getSlime) resolveSlime p ]

testESU :: KnownNat n => Proxy (BoardESU n) -> TestTree
testESU p = testGroup "BoardESU" $
  [ nonTrivEnds p
  , nonTrivSlime p
  , nonTrivUnits p ]
  ++ fmap (after AllSucceed "BoardESU.Nontrivial")
  [ fieldId "Ends identical after resolve units" getEnds resolveUnits p
  , fieldId "Units identical after resolve units" getUnits resolveUnits p ]

testSU :: KnownNat n => Proxy (BoardSU n) -> TestTree
testSU p = testGroup "BoardSU" $
  [ nonTrivSlime p
  , nonTrivUnits p ]
  ++ fmap (after AllSucceed "BoardSU.Nontrivial")
  [ fieldId "Slime indentical after resolveDeaths" getSlime resolveDeaths p ]

testEUO :: KnownNat n => Proxy (BoardEUO n) -> TestTree
testEUO p = testGroup "BoardEUO" $
  [ nonTrivEnds p
  , nonTrivUnits p
  , nonTrivOrders p ]
  ++ fmap (after AllSucceed "BoardEUO.Nontrivial")
  [ fieldId "Units conserved after resolveOrders"
    (sort . toList . getUnits) resolveOrders p ]

