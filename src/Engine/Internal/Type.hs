{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , Ends
  , Slime (..)
  , Unit (..)
  , Order (..)
  , HasNodes (..)
  , HNodes
  , HasEnds (..)
  , HEnds
  , HasSlime (..)
  , HSlime
  , HasUnits (..)
  , HUnits
  , HasOrders (..)
  , HOrders
  , getNodes
  , getEnds
  , getSlime
  , getUnits
  , getOrders
  ) where

import Engine.Util.MaybeNat

import           Control.Lens.Combinators
import           Data.Default.Class       (Default (..))
import           Data.Hashable            (Hashable)
import           Data.HashMap.Lazy        (HashMap)
import qualified Data.HashMap.Lazy        as HM
import           Data.HashSet             (HashSet)
import           Data.String              (IsString)
import           Data.Text                (Text)
import           GHC.Generics

newtype Node = Node {nodeName :: Text}
  deriving newtype (Eq, Hashable, IsString, Ord, Show)
  deriving stock Generic

type NodeAttr = HashMap Node

newtype Slime = Slime {slimeVal :: MaybeNat}
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show, Read)
  deriving stock Generic
instance Default Slime where def = Slime 0

type Ends = HashMap Node Int
instance Default (HashMap a b) where def = HM.empty

data Unit = Sprayer | Lobber deriving (Eq, Generic, Ord, Show)

data Order = Order { src :: Node, dest :: Node}
  deriving (Eq, Generic, Hashable, Show)

data Dummy = Dummy
  { _nodes  :: HashSet Node
  , _ends   :: NodeAttr Ends
  , _slime  :: NodeAttr Slime
  , _units  :: NodeAttr (Maybe Unit)
  , _orders :: NodeAttr (Maybe Node) }

$(makeFieldsNoPrefix ''Dummy)

instance HasNodes (HashSet Node) (HashSet Node) where nodes = id

type HNodes s = HasNodes s (HashSet Node)
type HEnds s = HasEnds s (NodeAttr Ends)
type HSlime s = HasSlime s (NodeAttr Slime)
type HUnits s = HasUnits s (NodeAttr (Maybe Unit))
type HOrders s = HasOrders s (NodeAttr (Maybe Node))

getNodes :: HNodes s => s -> HashSet Node
getNodes = view nodes

getEnds :: HEnds s => s -> NodeAttr Ends
getEnds = view ends

getSlime :: HSlime s => s -> NodeAttr Slime
getSlime = view slime

getUnits :: HUnits s => s -> NodeAttr (Maybe Unit)
getUnits = view units

getOrders :: HOrders s => s -> NodeAttr (Maybe Node)
getOrders = view orders

