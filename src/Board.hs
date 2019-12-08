{-# LANGUAGE TemplateHaskell #-}

module Board
  ( Board
  , makeBoard
  , emptyBoard
  , boardAttrs
  , renameNodes
  ) where

import Engine.Internal.Type
import Engine.PackAttr

import           Control.Lens.Combinators
import           Control.Monad            (liftM5)
import qualified Data.HashMap.Lazy        as HM
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HS

data Board = Board
  { _nodes  :: HashSet Node
  , _edges  :: NodeAttr [Node]
  , _slime  :: NodeAttr Slime
  , _units  :: NodeAttr (Maybe Unit)
  , _orders :: NodeAttr (Maybe Node)
  } deriving (Eq, Show)

$(makeFieldsNoPrefix ''Board)

makeBoard :: HashSet Node -> NodeAttr [Node] -> NodeAttr Slime
  -> NodeAttr (Maybe Unit) -> NodeAttr (Maybe Node) -> Board
makeBoard _nodes _edges _slime _units _orders =
  packAttr _orders
  . packAttr _units
  . packAttr _slime
  . packAttr _edges
  $ set nodes _nodes emptyBoard

emptyBoard :: Board
emptyBoard = Board
  { _nodes = HS.empty
  , _edges = HM.empty
  , _slime = HM.empty
  , _units = HM.empty
  , _orders = HM.empty }

boardAttrs :: Board -> (HashSet Node, NodeAttr [Node], NodeAttr Slime
  , NodeAttr (Maybe Unit), NodeAttr (Maybe Node))
boardAttrs b = (_nodes b, _edges b, _slime b, _units b, _orders b)

renameNodes :: (Node -> Node) -> Board -> Board
renameNodes f = liftM5 makeBoard
  (HS.map f . getNodes)
  (HM.fromList . fmap (\(n,ns) -> (f n, fmap f ns)) . HM.toList . getEdges)
  (HM.fromList . fmap (\(n,s) -> (f n, s)) . HM.toList . getSlime)
  (HM.fromList . fmap (\(n,u) -> (f n, u)) . HM.toList . getUnits)
  (HM.fromList . fmap (\(n,o) -> (f n, fmap f o)) . HM.toList . getOrders)


