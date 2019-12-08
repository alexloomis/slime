{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Board.Printer where

import Board
import Engine

import           Control.Monad     (liftM3)
import           Data.GraphViz
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as LT

instance Labellable Slime where
  toLabelValue = toLabelValue . show
deriving instance PrintDot Node

nodeLs :: Board -> [(Node, Slime)]
nodeLs b = fmap (\n -> (n, getOrDefault getSlime n b)) . HS.toList . getNodes $ b

isOrder :: Board -> Node -> Node -> Bool
isOrder b n1 n2
  | getOrDefault getOrders n1 b == Just n2 = True
  | otherwise = False

edgeLs :: Board -> [(Node, Node, Bool)]
edgeLs b = concatMap labeler . HM.toList . getEdges $ b
  where labeler (n,ns) = [ (n, n2, isOrder b n n2) | n2 <- ns ]

graphParams :: Board -> GraphvizParams Node Slime Bool () Slime
graphParams b = nonClusteredParams
  { globalAttributes = globals
  , fmtNode = nodeFormatter b
  , fmtEdge = edgeFormatter b }

globals :: [GlobalAttributes]
globals = []

nodeFormatter :: Board -> (Node, Slime) -> Attributes
nodeFormatter b (n,s) =
  [ toLabel s
  , shape $ case getOrDefault getUnits n b of
      Nothing      -> Ellipse
      Just Lobber  -> Square
      Just Sprayer -> Triangle ]

edgeFormatter :: Board -> (Node, Node, Bool) -> Attributes
edgeFormatter _ (_,_,tf) =
  [ if tf then arrowTo diamond else arrowTo normal ]

-- |pack . show changes text from unicode characters to escape sequences
boardToDot :: Board -> DotGraph Node
boardToDot = liftM3 graphElemsToDot graphParams nodeLs edgeLs
  . renameNodes (\(Node t) -> Node $ "N: " `T.append` (T.pack . show $ t))

printBoard :: Board -> IO ()
printBoard = LT.putStrLn . printDotGraph . boardToDot

