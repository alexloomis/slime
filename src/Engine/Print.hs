{-# LANGUAGE OverloadedStrings #-}
module Engine.Print where

import Prelude hiding (unlines)

import Engine.Internal.Type
import Engine.Util.MaybeNat

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import           Data.Text         (Text, append, pack, unlines)
import qualified Data.Text         as T

printNodes :: HNodes s => s -> Text
printNodes = unlines . (:) "[NODES]"
  . fmap nodeName . HS.toList . getNodes

printEnds :: HEnds s => s -> Text
printEnds = unlines . (:) "[EDGES]"
  . concatMap f . HM.toList . getEnds
  where
    f (n, es) = if es == HM.empty
      then []
      else (nodeName n `append` " ->")
        : fmap showMult (HM.toList es)
    showMult (k,m) = case m of
      1 -> "  " `append` nodeName k
      _ -> "  "
        `append` nodeName k
        `append` " # "
        `append` (T.pack . show $ m)

printSlime :: HSlime s => s -> Text
printSlime = unlines . (:) "[SLIME]"
  . fmap showSlime . HM.toList . getSlime
  where
    showSlime (n, Slime (MNat s)) =
      nodeName n `append` " -> " `append` (pack . show $ s)
    showSlime (n, Slime MErr) =
      nodeName n `append` " -> Err"

printUnits :: HUnits s => s -> Text
printUnits = unlines . (:) "[UNITS]"
  . fmap showUnit . HM.toList . HM.mapMaybe id . getUnits
  where
    showUnit (n,s) = nodeName n `append` " -> " `append` (pack . show $ s)

printOrders :: HOrders s => s -> Text
printOrders = unlines . (:) "[ORDERS]"
  . fmap showOrder . HM.toList . HM.mapMaybe id . getOrders
  where
    showOrder (n,s) = nodeName n `append` " -> " `append` nodeName s

