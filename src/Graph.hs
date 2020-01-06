{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Graph where

import Engine
import Internal.Util (toMap)

import Control.Monad     (liftM3)
import Data.GraphViz
import Data.Text         (Text)
import Data.Text.Lazy    (toStrict)
import Data.Vector.Sized (index)
import GHC.TypeNats      (KnownNat)
import Numeric.Natural   (Natural)

instance Labellable Natural where
  toLabelValue = toLabelValue . show
instance Labellable Slime where
  toLabelValue (Slime n) = toLabelValue n
instance Labellable (NodeID n, Slime) where
  toLabelValue (n, Slime s) = toLabelValue $ show n ++ ":" ++ show s
instance KnownNat n => PrintDot (NodeID n) where
  unqtDot = unqtDot . toInteger

nodeLs :: HSlime s n => s -> [(NodeID n, Slime)]
nodeLs = toMap . getSlime

isOrder :: HOrders s n => s -> NodeID n -> NodeID n -> Bool
isOrder s n1 n2
  | getOrders s `index` n1 == Order (Just n2) = True
  | otherwise = False

endLs :: HEnds s n => s -> [(NodeID n, NodeID n, Natural)]
endLs = concatMap flatten . toMap . getEnds
  where flatten (n1, kvs) = [ (n1, n2, m) | (n2, m) <- toMap kvs , m /= 0 ]

globals :: [GlobalAttributes]
globals = []

nodeFormatter :: HUnits s n => s -> (NodeID n, Slime) -> Attributes
nodeFormatter b (n,s) =
  [ toLabel (n,s)
  , shape $ case getUnits b `index` n of
      Nothing      -> Ellipse
      Just Lobber  -> Square
      Just Sprayer -> Triangle ]

endFormatter :: HOrders s n => s -> (NodeID n, NodeID n, Natural) -> Attributes
endFormatter b (n1, n2, m) =
  [ if isOrder b n1 n2 then arrowTo diamond else arrowTo normal
  , toLabel m ]

graphParams :: (HUnits s n, HOrders s n)
  => s -> GraphvizParams (NodeID n) Slime Natural () Slime
graphParams b = nonClusteredParams
  { globalAttributes = globals
  , fmtNode = nodeFormatter b
  , fmtEdge = endFormatter b }

boardToDot :: (HEnds s n, HSlime s n, HUnits s n, HOrders s n)
  => s -> DotGraph (NodeID n)
boardToDot = liftM3 graphElemsToDot graphParams nodeLs endLs

printGraph :: (KnownNat n, HEnds s n, HSlime s n, HUnits s n, HOrders s n)
  => s -> Text
printGraph = toStrict . printDotGraph . boardToDot

