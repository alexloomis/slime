{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Board.Graph where

import Engine

import           Control.Monad     (liftM3)
import           Data.GraphViz
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.Text.Lazy.IO as LT

instance Labellable Slime where
  toLabelValue = toLabelValue . show
deriving instance PrintDot Node

nodeLs :: (HNodes s, HSlime s) => s -> [(Node, Slime)]
nodeLs b = fmap (\n -> (n, getOrDefault getSlime n b)) . HS.toList . getNodes $ b

isOrder :: HOrders s => s -> Node -> Node -> Bool
isOrder b n1 n2
  | getOrDefault getOrders n1 b == Just n2 = True
  | otherwise = False

endLs :: HEnds s => s -> [(Node, Node, Int)]
endLs = concatMap flatten . HM.toList . HM.map HM.toList . getEnds
  where
    flatten (n1, kvs) =
      [ (n1, n2, m)
      | (n2,m) <- kvs ]

graphParams :: (HUnits s, HOrders s)
  => s -> GraphvizParams Node Slime Int () Slime
graphParams b = nonClusteredParams
  { globalAttributes = globals
  , fmtNode = nodeFormatter b
  , fmtEdge = endFormatter b }

globals :: [GlobalAttributes]
globals = []

nodeFormatter :: HUnits s => s -> (Node, Slime) -> Attributes
nodeFormatter b (n,s) =
  [ toLabel s
  , shape $ case getOrDefault getUnits n b of
      Nothing      -> Ellipse
      Just Lobber  -> Square
      Just Sprayer -> Triangle ]

endFormatter :: HOrders s => s -> (Node, Node, Int) -> Attributes
endFormatter b (n1,n2,m) =
  [ if isOrder b n1 n2 then arrowTo diamond else arrowTo normal
  , toLabel m ]

-- |pack . show changes text from unicode characters to escape sequences
boardToDot :: (HNodes s, HEnds s, HSlime s, HUnits s, HOrders s)
  => s -> DotGraph Node
boardToDot = liftM3 graphElemsToDot graphParams nodeLs endLs

printBoard :: (HNodes s, HEnds s, HSlime s, HUnits s, HOrders s)
  => s -> IO ()
printBoard = LT.putStrLn . printDotGraph . boardToDot

