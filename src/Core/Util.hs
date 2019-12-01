{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Util where

import Core.Type

import           Control.Lens.Combinators
import           Control.Monad.State
import           Data.Default.Class       (Default (..))
import           Data.Hashable            (Hashable)
import           Data.HashMap.Lazy        (HashMap, (!))
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as Set

packState :: (a -> State b c) -> State b (a -> c)
packState f = state . mapToFst . flip $ evalState . f
  where mapToFst g a = (g a, a)

unpackState :: State b (a -> c) -> a -> State b c
unpackState s a = mapState (applyToFst a) s
  where applyToFst c (f, b) = (f c, b)

divOrZero :: Integral a => a -> a -> a
divOrZero num = \case
    0 -> 0
    x -> div num x

-- |Packs values, filtering out keys that do not correspond to nodes,
-- adding Default value for missing nodes.
class SafePack m a where
  packAttr :: MonadState r m => NodeAttr a -> m (NodeAttr a)
  default packAttr :: (MonadState r m, HNodes r, Default a)
    => NodeAttr a -> m (NodeAttr a)
  packAttr a = do
    ns <- gets (HM.map (const def) . Set.toMap . view nodes)
    let attrMap = HM.intersection a ns
    return $ HM.union attrMap ns

instance (MonadState r m, HNodes r) => SafePack m (Maybe Unit)
instance (MonadState r m, HNodes r) => SafePack m Slime
instance (MonadState r m, HNodes r) => SafePack m [Node] where
  packAttr a = do
    nodeSet <- gets (view nodes)
    ns <- gets (HM.map (const def) . Set.toMap . view nodes)
    let attrMap = HM.intersectionWith
          (\v _ -> filter (`Set.member` nodeSet) v) a ns
    return $ HM.union attrMap ns

-- |Specialises to, e.g.,
-- attrAt :: (a -> NodeAttr Slime) -> Node -> State a Slime
-- so in
-- a <- attr hasSlime node,
-- a :: Slime.
attr :: (Eq k, Hashable k, MonadState a m)
  => (a -> HashMap k b) -> k -> m b
attr projection node = flip (!) node <$> gets projection

