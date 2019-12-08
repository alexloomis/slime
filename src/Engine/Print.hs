{-# LANGUAGE OverloadedStrings #-}
module Engine.Print where

import Engine.Internal.Type

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.Text         as T

printNodes :: HNodes s => s -> IO ()
printNodes = print . T.unlines . (:) "NODES:"
  . fmap nodeName . HS.toList . getNodes

printEdges :: HEdges s => s -> IO ()
printEdges = print . T.unlines . (:) "EDGES:"
  . concatMap f . HM.toList . getEdges
  where
    f (n,ns) =
      [ "FROM:"
      , nodeName n
      , "TO:" ]
      ++ fmap nodeName ns

printSlime :: HSlime s => s -> IO ()
printSlime = print . T.unlines . (:) "SLIME:"
  . concatMap f . HM.toList . getSlime
  where
    f (n,s) =
      [ "AT:"
      , nodeName n
      , "AMNT:"
      , T.pack . show $ s ]

printUnits :: HUnits s => s -> IO ()
printUnits = print . T.unlines . (:) "UNITS:"
  . concatMap f . HM.toList . getUnits
  where
    f (n,u) =
      [ "AT:"
      , nodeName n
      , "UNIT:"
      , T.pack . show $ u ]

printOrders :: HOrders s => s -> IO ()
printOrders = print . T.unlines . (:) "ORDERS:"
  . concatMap f . HM.toList . getOrders
  where
    f (n,o) =
      [ "FROM:"
      , nodeName n
      , "TO:"
      , T.pack . show $ o ]


