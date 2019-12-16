{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Test.Gen () where

import Board
import Engine
import Engine.Util.MaybeNat

import           Control.Monad
import qualified Data.HashMap.Lazy                as HM
import qualified Data.HashSet                     as HS
import           Data.List
import qualified Data.Text                        as T
import           Test.Framework.QuickCheckWrapper
import           Test.QuickCheck.Gen.Unsafe       (promote)
import           Test.QuickCheck.Instances        ()

attrGen :: Arbitrary a => [Node] -> Gen (NodeAttr a)
attrGen ns = do
  chosenNs <- shuffle ns >>= sublistOf
  aVals <- arbitrary
  return . HM.fromList . zip chosenNs $ aVals

endsPreGen :: [Node] -> Gen Ends
endsPreGen ns = do
  ends <- shuffle ns >>= sublistOf
  vals <- arbitrary
  return . HM.fromList . zip ends $ vals

endsGen :: [Node] -> Gen (NodeAttr Ends)
endsGen ns = do
  starts <- shuffle ns >>= sublistOf
  ends <- listOf $ endsPreGen ns
  return . HM.fromList . zip starts $ ends

allUnique :: Eq a => [a] -> Bool
allUnique x = x == nub x

takeHead :: [[a]] -> [a]
takeHead []    = []
takeHead (x:_) = x

takeUntilUnique :: [T.Text] -> [T.Text]
takeUntilUnique [] = []
takeUntilUnique ts =
  takeHead . dropWhile (not . allUnique) . transpose . fmap T.inits . nub $ ts

liftToNodeList :: ([T.Text] -> [T.Text]) -> [Node] -> [Node]
liftToNodeList f = fmap Node . f . fmap nodeName

orderGen :: [Node] -> Gen (NodeAttr (Maybe Node))
orderGen ns = do
  starts <- shuffle ns >>= sublistOf
  ends <- listOf . liftArbitrary . elements $ ns
  return . HM.fromList . zip starts $ ends

instance Arbitrary MaybeNat where
  arbitrary = frequency [(1, return MErr), (100, fmap MNat arbitrary)]
  shrink MErr = []
  shrink x    = [MErr .. x-1]
deriving instance Arbitrary Node
deriving instance Arbitrary Slime
instance Arbitrary Unit where
  arbitrary = elements [Lobber, Sprayer]
instance Arbitrary Order where
  arbitrary = liftM2 Move arbitrary arbitrary
instance Arbitrary Board where
  arbitrary = arbitrary >>= (liftM5 . liftM5) makeBoard
    (return . HS.fromList) endsGen attrGen attrGen orderGen
  shrink b = if b == emptyBoard
    then []
    else [ makeBoard ns' es sl un od
      | let (ns, es, sl, un, od) = boardAttrs b
      , ns' <- (delete ns . drop 1 . fmap HS.fromList
        . subsequences . HS.toList $ ns) ++ [HS.empty] ]

instance Function MaybeNat
instance Function Node
instance Function Slime
instance Function Unit
instance Function Order

