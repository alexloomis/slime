{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-
module Core.Type
  ( Node (..)
  , HasNodes
  , getNodes
  , NodeAttr
  , HasEdges
  , getEdges
  , SafePack (..)
  , attr
  ) where
-}
module Core.Type where

import Core.Util.MaybeNat

import Control.Lens.Combinators
import Data.Default.Class       (Default (..))
import Data.Hashable            (Hashable)
import Data.HashMap.Lazy        (HashMap)
import Data.HashSet             (HashSet)
import Data.String              (IsString)
import Data.Text                (Text)

newtype Node = Node {nodeName :: Text}
  deriving (Eq, Hashable, IsString, Ord, Show)

type NodeAttr = HashMap Node

newtype Slime = Slime MaybeNat
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Read)
instance Default Slime where def = Slime 0

data Unit = Sprayer | Lobber deriving (Eq, Show)

data Dummy = Dummy
  { _nodes :: HashSet Node
  , _edges :: NodeAttr [Node]
  , _slime :: NodeAttr Slime
  , _units :: NodeAttr (Maybe Unit) }

$(makeFieldsNoPrefix ''Dummy)

instance HasNodes (HashSet Node) (HashSet Node) where
  nodes = id

type HNodes a = HasNodes a (HashSet Node)
type HEdges a = HasEdges a (NodeAttr [Node])
type HSlime a = HasSlime a (NodeAttr Slime)
type HUnits a = HasUnits a (NodeAttr (Maybe Unit))

