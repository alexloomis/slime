{-# LANGUAGE OverloadedStrings #-}
module Internal.Print where

import Prelude hiding (unlines)

import Internal.Import

import Data.Finite (getFinite)
import Data.Text   (Text, append, pack, unlines)

showT :: Show a => a -> Text
showT = pack . show

showTF :: Finite n -> Text
showTF = pack . show . getFinite

printEnds :: HEnds s n => s -> Text
printEnds = unlines . filter (/= "")
  . (:) "[ENDS]" . concatMap f . toMap . view ends
  where
    f (n, es) = if all (== 0) $ toList es
      then []
      else (showTF n `append` " ->") : fmap showMult (toMap es)
    showMult (k,m) = if m == 0
      then ""
      else "  " `append` showTF k `append` " # " `append` showT m

printSlime :: HSlime s n => s -> Text
printSlime = unlines . filter (/= "")
  .  (:) "[SLIME]" . fmap showSlime . toMap . view slime
  where
    showSlime (n, Slime s) = if s == 0
      then ""
      else showTF n `append` " -> " `append` showT s

printUnits :: HUnits s n => s -> Text
printUnits = unlines . filter (/= "")
  . (:) "[UNITS]" . fmap showUnit . toMap . view units
  where
    showUnit (n, Just s)  = showTF n `append` " -> " `append` showT s
    showUnit (_, Nothing) = ""

printOrders :: HOrders s n => s -> Text
printOrders = unlines . filter (/= "")
  . (:) "[ORDERS]" . fmap showOrder . toMap . view orders
  where
    showOrder (n, Order (Just s)) = showTF n `append` " -> " `append` showTF s
    showOrder (_, Order Nothing)  = ""

