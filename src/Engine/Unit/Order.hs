{-# LANGUAGE OverloadedStrings #-}

module Engine.Unit.Order
  ( giveOrder
  , checkOrder
  , clearOrderFrom
  , clearAllOrders
  , attrToOrders
  ) where

import Engine.Internal.Type
import Engine.Internal.Util

import           Control.Lens.Combinators
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as Set
import           Data.Maybe               (catMaybes, isNothing, mapMaybe)
import           Data.Text                (Text, append)

type ErrCode = (Int, Text)

checkCmdSrc :: (HNodes s, MonadReader s m, MonadError ErrCode m)
  => Order -> m Order
checkCmdSrc cmd = do
  ns <- asks $ view nodes
  if src cmd `Set.member` ns
    then return cmd
    else throwError (-1, "Order source is not a valid node.")

checkCmdDest :: (HNodes s, MonadReader s m, MonadError ErrCode m)
  => Order -> m Order
checkCmdDest cmd = do
  ns <- asks $ view nodes
  if dest cmd `Set.member` ns
    then return cmd
    else throwError (-2, "Order destination is not a valid node.")

checkSrcUnit :: (HUnits s, MonadReader s m, MonadError ErrCode m)
  => Order -> m Order
checkSrcUnit cmd = do
  unit <- asks $ valOrDefault units (src cmd)
  if isNothing unit
    then throwError (-3, "There is no unit at "
      `append` nodeName (src cmd) `append` ".")
    else return cmd

checkAlreadyCmd  :: (HOrders s, MonadReader s m, MonadError ErrCode m)
  => Order -> m Order
checkAlreadyCmd cmd = do
  allCmds <- asks $ view orders
  if HM.lookup (src cmd) allCmds == Just Nothing
    then return cmd
    else throwError (-4, "The unit at " `append` nodeName (src cmd)
      `append` " has already been given a command.")

checkHold :: MonadError ErrCode m => Order -> m Order
checkHold cmd = if src cmd == dest cmd
  then throwError (-5, "There's no need to order a unit to hold.")
  else return cmd

checkEnd :: (HEnds s, MonadReader s m, MonadError ErrCode m)
  => Order -> m Order
checkEnd cmd = do
  es <- asks $ HM.keysSet . valOrDefault ends (src cmd)
  if dest cmd `Set.member` es
    then return cmd
    else throwError (-6, "There is no edge from " `append` nodeName (src cmd)
      `append` " to " `append` nodeName (dest cmd) `append` ".")

checkAlreadyDest :: (HOrders s, MonadReader s m, MonadError ErrCode m)
  => Order -> m Order
checkAlreadyDest cmd = do
  allCmds <- asks $ catMaybes . HM.elems . view orders
  if dest cmd `elem` allCmds
    then throwError (-7, "There is already a unit moving to "
      `append` nodeName (dest cmd) `append` ".")
    else return cmd

checkOrder :: (HNodes s, HEnds s, HUnits s, HOrders s)
  => Order -> s -> Either ErrCode Order
checkOrder cmd =
  runReader . runExceptT $
  checkCmdSrc cmd
  >>= checkCmdDest
  >>= checkSrcUnit
  >>= checkAlreadyCmd
  >>= checkHold
  >>= checkEnd
  >>= checkAlreadyDest

giveOrder :: (HNodes s, HEnds s, HUnits s, HOrders s) => Order -> s -> s
giveOrder cmd@(Order s d) = liftM3 either const
  (const . over orders (HM.insert s (Just d))) (checkOrder cmd)

clearOrderFrom :: HOrders s => Node -> s -> s
clearOrderFrom node = over orders $ HM.adjust (const Nothing) node

clearAllOrders :: HOrders s => s -> s
clearAllOrders = over orders $ HM.map (const Nothing)

attrToOrders :: NodeAttr (Maybe Node) -> [Order]
attrToOrders = mapMaybe f . HM.toList
  where
    f (a, Just b) = Just $ Order a b
    f _           = Nothing

