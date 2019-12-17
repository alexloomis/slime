{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Board
  ( Board
  , makeBoard
  , emptyBoard
  , boardAttrs
  , renameNodes
  , printBoard
  ) where

import Board.Graph
import Engine
import Engine.Internal.Type
import Engine.PackAttr      ()
import GameState
import Parser.Save

import           Control.Lens.Combinators
import           Control.Monad            (liftM5)
import qualified Data.HashMap.Lazy        as HM
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HS

data Board = Board
  { _nodes  :: HashSet Node
  , _ends   :: NodeAttr Ends
  , _slime  :: NodeAttr Slime
  , _units  :: NodeAttr (Maybe Unit)
  , _orders :: NodeAttr (Maybe Node)
  } deriving (Eq, Show)

$(makeFieldsNoPrefix ''Board)

makeBoard :: HashSet Node -> NodeAttr Ends -> NodeAttr Slime
  -> NodeAttr (Maybe Unit) -> NodeAttr (Maybe Node) -> Board
makeBoard _nodes _ends _slime _units _orders =
  packAttr _orders
  . packAttr _units
  . packAttr _slime
  . packAttr _ends
  $ set nodes _nodes emptyBoard

emptyBoard :: Board
emptyBoard = Board
  { _nodes = HS.empty
  , _ends = HM.empty
  , _slime = HM.empty
  , _units = HM.empty
  , _orders = HM.empty }

boardAttrs :: Board -> (HashSet Node, NodeAttr Ends, NodeAttr Slime
  , NodeAttr (Maybe Unit), NodeAttr (Maybe Node))
boardAttrs b = (_nodes b, _ends b, _slime b, _units b, _orders b)

renameKeys :: (Node -> Node) -> NodeAttr a -> NodeAttr a
renameKeys f = HM.fromList . fmap (\(k,v) -> (f k, v)) . HM.toList

renameNodes :: (Node -> Node) -> Board -> Board
renameNodes f = liftM5 makeBoard
  (HS.map f . getNodes)
  (HM.map (renameKeys f) . renameKeys f . getEnds)
  (renameKeys f . getSlime)
  (renameKeys f . getUnits)
  (HM.map (fmap f) . renameKeys f . getOrders)

instance GameState Board where
  parseGame = uncurry5 makeBoard <$> parseSave
    where uncurry5 f (a,b,c,d,e) = f a b c d e

