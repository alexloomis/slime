{-# LANGUAGE OverloadedStrings #-}

module Engine.Unit.Order
  ( giveOrder
  , checkOrder
  , clearOrderFrom
  , clearAllOrders
  , attrToOrders
  ) where

import Engine.Internal.Type

import           Control.Lens.Combinators
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.HashMap.Lazy        ((!))
import qualified Data.HashMap.Lazy        as HM
import qualified Data.HashSet             as Set
import           Data.Maybe               (catMaybes, isNothing, mapMaybe)
import           Data.Text                (Text, append)

checkSrcUnit :: (HUnits s, MonadReader s m, MonadError Text m)
  => Order -> m Order
checkSrcUnit cmd = do
  unit <- asks $ val units (src cmd)
  if isNothing unit
    then throwError $ "There is no unit at "
      `append` nodeName (src cmd) `append` "."
    else return cmd

checkAlreadyCmd  :: (HOrders s, MonadReader s m, MonadError Text m)
  => Order -> m Order
checkAlreadyCmd cmd = do
  allCmds <- asks $ view orders
  if isNothing $ allCmds ! src cmd
    then return cmd
    else throwError $ "The unit at " `append` nodeName (src cmd)
      `append` " has already been given a command."

checkCmdSrc :: (HNodes s, MonadReader s m, MonadError Text m)
  => Order -> m Order
checkCmdSrc cmd = do
  ns <- asks $ view nodes
  if src cmd `Set.member` ns
    then return cmd
    else throwError "Order source is not a valid node."

checkCmdDest :: (HNodes s, MonadReader s m, MonadError Text m)
  => Order -> m Order
checkCmdDest cmd = do
  ns <- asks $ view nodes
  if dest cmd `Set.member` ns
    then return cmd
    else throwError "Order destination is not a valid node."

checkHold :: MonadError Text m => Order -> m Order
checkHold cmd = if src cmd == dest cmd
  then throwError "There's no need to order a unit to hold."
  else return cmd

checkEdge :: (HEdges s, MonadReader s m, MonadError Text m)
  => Order -> m Order
checkEdge cmd = do
  es <- asks $ val edges (src cmd)
  if dest cmd `elem` es
    then return cmd
    else throwError $ "There is no edge from " `append` nodeName (src cmd)
      `append` " to " `append` nodeName (dest cmd) `append` "."

checkAlreadyDest :: (HOrders s, MonadReader s m, MonadError Text m)
  => Order -> m Order
checkAlreadyDest cmd = do
  allCmds <- asks $ catMaybes . HM.elems . view orders
  if dest cmd `elem` allCmds
    then throwError $ "There is already a unit moving to "
      `append` nodeName (dest cmd) `append` "."
    else return cmd

checkOrder :: (HNodes s, HEdges s, HUnits s, HOrders s)
  => Order -> s -> Either Text Order
checkOrder cmd =
  runReader . runExceptT $
  checkSrcUnit cmd
  >>= checkAlreadyCmd
  >>= checkCmdSrc
  >>= checkCmdDest
  >>= checkHold
  >>= checkEdge
  >>= checkAlreadyDest

giveOrder :: (HNodes s, HEdges s, HUnits s, HOrders s) => Order -> s -> s
giveOrder cmd@(Move s d) = liftM3 either const
  (const . over orders (HM.insert s (Just d))) (checkOrder cmd)

clearOrderFrom :: HOrders s => Node -> s -> s
clearOrderFrom node = over orders $ HM.adjust (const Nothing) node

clearAllOrders :: HOrders s => s -> s
clearAllOrders = over orders $ HM.map (const Nothing)

attrToOrders :: NodeAttr (Maybe Node) -> [Order]
attrToOrders = mapMaybe f . HM.toList
  where
    f (a, Just b) = Just $ Move a b
    f _           = Nothing

