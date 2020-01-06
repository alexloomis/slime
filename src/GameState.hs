module GameState
  ( GameState (..)
  , Victory (..)
  ) where

import Engine
import Graph
import Internal.Print

import Control.Monad.Identity (Identity)
import Data.Text              (Text, unlines)
import GHC.TypeNats          (KnownNat)
import Data.Void              (Void)
import Text.Megaparsec

type Parser = ParsecT Void Text Identity
data Victory = Win | Lose | Ongoing deriving Show

-- |Instances should obey `runParser parseGame . showGame = id`.
class (HEnds s n, HSlime s n, HUnits s n, HOrders s n, KnownNat n)
  => GameState s n where
  endTurn :: s -> s
  victory :: s -> Victory
  parseGame :: Parser s
  showGame :: s -> Text
  graphGame :: s -> Text
  endTurn = resolveDeaths . resolveSlime . resolveUnits . resolveOrders
  showGame s = Data.Text.unlines
    [printEnds s, printSlime s, printUnits s, printOrders s]
  graphGame = printGraph

