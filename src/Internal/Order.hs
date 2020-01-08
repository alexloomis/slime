{-# LANGUAGE DeriveGeneric #-}
module Internal.Order where

import Internal.Import

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe           (isJust, isNothing, mapMaybe)
import Data.Text            (Text, append, pack)
import GHC.Generics         (Generic)

data ErrOrder =
    NoUnit
  | AlreadyCmd
  | NoEdge
  | DestConflict

type ErrCode = (ErrOrder, Text)

data OneOrder n = OneOrder {src :: NodeID n, dest :: NodeID n}
  deriving (Eq, Generic, Show)

srcName :: OneOrder n -> Text
srcName = pack . show . src

destName :: OneOrder n -> Text
destName = pack . show . dest

checkSrcUnit :: (HUnits s n, MonadReader s m, MonadError ErrCode m)
  => OneOrder n -> m (OneOrder n)
checkSrcUnit cmd = do
  unit <- asks $ flip (viewAt units) (src cmd)
  if isNothing unit
    then throwError (NoUnit,
      "There is no unit at " `append` srcName cmd `append` ".")
    else return cmd

checkAlreadyCmd  :: (HOrders s n, MonadReader s m, MonadError ErrCode m)
  => OneOrder n -> m (OneOrder n)
checkAlreadyCmd cmd = do
  allCmds <- asks $ fmap _order . view orders
  if isNothing $ allCmds `index` src cmd
    then return cmd
    else throwError (AlreadyCmd,
      "The unit at " `append` srcName cmd
      `append` " has already been given a command.")

{-
checkHold :: MonadError ErrCode m => OneOrder n -> m (OneOrder n)
checkHold cmd = if src cmd == dest cmd
  then throwError (-3, "There's no need to order a unit to hold.")
  else return cmd
-}

checkEnd :: (HEnds s n, MonadReader s m, MonadError ErrCode m)
  => OneOrder n -> m (OneOrder n)
checkEnd cmd = do
  es <- asks $ flip (viewAt ends) (src cmd)
  if es `index` dest cmd == 0
    then throwError (NoEdge,
      "There is no edge from " `append` srcName cmd
      `append` " to " `append` destName cmd `append` ".")
    else return cmd

checkAlreadyDest :: (HOrders s n, MonadReader s m, MonadError ErrCode m)
  => OneOrder n -> m (OneOrder n)
checkAlreadyDest cmd = do
  allCmds <- asks $ mapMaybe _order . toList . view orders
  if dest cmd `elem` allCmds
    then throwError (DestConflict,
      "There is already a unit moving to " `append` destName cmd `append` ".")
    else return cmd

checkOrder :: (HEnds s n, HUnits s n, HOrders s n)
  => OneOrder n -> s -> Either ErrCode (OneOrder n)
checkOrder cmd =
  runReader . runExceptT $
  checkSrcUnit cmd
  >>= checkAlreadyCmd
  -- >>= checkHold
  >>= checkEnd
  >>= checkAlreadyDest

hasOrder :: Order n -> Bool
hasOrder (Order n) = isJust n

giveOrder :: (HEnds s n, HUnits s n, HOrders s n) => OneOrder n -> s -> s
giveOrder cmd s = case checkOrder cmd s of
  Left _               -> s
  Right (OneOrder a b) -> over orders (setElem a . Order . Just $ b) s

clearOrderFrom :: HOrders s n => NodeID n -> s -> s
clearOrderFrom n = over orders $ setElem n (Order Nothing)

clearAllOrders :: HOrders s n => s -> s
clearAllOrders = over orders $ fmap (const (Order Nothing))

toOneOrders :: Vector n (Order n) -> [OneOrder n]
toOneOrders = mapMaybe extract . toMap
  where
    extract (_, Order Nothing)  = Nothing
    extract (n, Order (Just m)) = Just $ OneOrder n m

