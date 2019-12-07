{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Engine.Internal.Type
  ( Node (..)
  , NodeAttr
  , Slime (..)
  , Unit (..)
  , Order (..)
  , HasNodes (..)
  , HNodes
  , HasEdges (..)
  , HEdges
  , HasSlime (..)
  , HSlime
  , HasUnits (..)
  , HUnits
  , HasOrders (..)
  , HOrders
  , getNodes
  , getEdges
  , getSlime
  , getUnits
  , getOrders
  , val
  ) where

import Engine.Util.MaybeNat

import Control.Lens.Combinators
import Data.Default.Class       (Default (..))
import Data.Hashable            (Hashable)
import Data.HashMap.Lazy        (HashMap, (!))
import Data.HashSet             (HashSet)
import Data.String              (IsString)
import Data.Text                (Text)
import GHC.Generics

newtype Node = Node {nodeName :: Text}
  deriving newtype (Eq, Hashable, IsString, Ord, Show)
  deriving stock Generic

type NodeAttr = HashMap Node

newtype Slime = Slime MaybeNat
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show, Read)
  deriving stock Generic
instance Default Slime where def = Slime 0

data Unit = Sprayer | Lobber deriving (Eq, Generic, Ord, Show)

data Order = Move { src :: Node, dest :: Node}
  deriving (Eq, Generic, Hashable, Show)

data Dummy = Dummy
  { _nodes  :: HashSet Node
  , _edges  :: NodeAttr [Node]
  , _slime  :: NodeAttr Slime
  , _units  :: NodeAttr (Maybe Unit)
  , _orders :: NodeAttr (Maybe Node) }

$(makeFieldsNoPrefix ''Dummy)

instance HasNodes (HashSet Node) (HashSet Node) where nodes = id

type HNodes a = HasNodes a (HashSet Node)
type HEdges a = HasEdges a (NodeAttr [Node])
type HSlime a = HasSlime a (NodeAttr Slime)
type HUnits a = HasUnits a (NodeAttr (Maybe Unit))
type HOrders a = HasOrders a (NodeAttr (Maybe Node))

getNodes :: HNodes s => s -> HashSet Node
getNodes = view nodes

getEdges :: HEdges s => s -> NodeAttr [Node]
getEdges = view edges

getSlime :: HSlime s => s -> NodeAttr Slime
getSlime = view slime

getUnits :: HUnits s => s -> NodeAttr (Maybe Unit)
getUnits = view units

getOrders :: HOrders s => s -> NodeAttr (Maybe Node)
getOrders = view orders

val :: Getting (NodeAttr v) s (NodeAttr v) -> Node -> s -> v
val field node s = view field s ! node

