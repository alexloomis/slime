{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

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

import           Control.Monad.State
import           Data.Default.Class  (Default (..))
import           Data.Hashable       (Hashable)
import           Data.HashMap.Lazy   (HashMap, (!))
import qualified Data.HashMap.Lazy   as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.String         (IsString)
import           Data.Text           (Text)
import           GHC.Records         (HasField, getField)
import           Lens.Simple

newtype Node = Node {nodeName :: Text}
  deriving (Eq, Hashable, IsString, Ord, Show)

instance HasField "nodes" (HashSet Node) (HashSet Node) where getField = id

type HasNodes r = HasField "nodes" r (HashSet Node)

getNodes :: HasNodes r => r -> HashSet Node
getNodes = getField @"nodes"

type NodeAttr = HashMap Node

instance HasField "nodes" (NodeAttr a) (HashSet Node)
  where getField = HM.keysSet

type HasEdges r = HasField "edges" r (NodeAttr [Node])

getEdges :: HasEdges r => r -> NodeAttr [Node]
getEdges = getField @"edges"

-- |Packs values, filtering out keys that do not correspond to nodes,
-- adding Default value for missing nodes.
class SafePack m a where
  packAttr :: MonadState r m => NodeAttr a -> m (NodeAttr a)
  default packAttr :: (MonadState r m, HasNodes r, Default a)
    => NodeAttr a -> m (NodeAttr a)
  packAttr a = do
    nodes <- gets (HM.map (const def) . Set.toMap . getNodes)
    let attrMap = HM.intersection a nodes
    return $ HM.union attrMap nodes

instance (MonadState r m, HasNodes r) => SafePack m [Node] where
  packAttr a = do
    nodeSet <- gets getNodes
    nodes <- gets (HM.map (const def) . Set.toMap . getNodes)
    let attrMap = HM.intersectionWith
          (\v _ -> filter (`Set.member` nodeSet) v) a nodes
    return $ HM.union attrMap nodes

-- |Specialises to, e.g.,
-- attrAt :: (a -> NodeAttr Slime) -> Node -> State a Slime
-- so in
-- a <- attr hasSlime node,
-- a :: Slime.
attr :: (Eq k, Hashable k, MonadState a m)
  => (a -> HashMap k b) -> k -> m b
attr projection node = flip (!) node <$> gets projection


