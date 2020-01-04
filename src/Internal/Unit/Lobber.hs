module Internal.Unit.Lobber where

import Internal.Import

lobberRange :: Nat
lobberRange = 1

maxNeighbors :: (HEnds s n, HSlime s n) => s -> NodeID n -> [NodeID n]
maxNeighbors s = maximaOn (viewAt slime s) . nthNeighbors lobberRange s

lobberTargets :: (HEnds s n, HUnits s n, HSlime s n) => s -> [NodeID n]
lobberTargets s = concat . toList $ imap onlyLobbers (s ^. ends)
  where
    onlyLobbers k _ = case viewAt units s k of
      Just Lobber -> maxNeighbors s k
      _           -> []

resolveLobber :: (HEnds s n, HSlime s n, HUnits s n) => s -> s
resolveLobber s = over slime (imap f) s
  where f k v = if k `elem` lobberTargets s then 0 else v

