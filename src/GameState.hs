module GameState
  ( GameState (..)
  ) where

import Engine
import Engine.Print

import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Void              (Void)
import           Text.Megaparsec

type Parser = ParsecT Void Text Identity

class (HNodes s, HEnds s, HSlime s, HUnits s, HOrders s) => GameState s where
  endTurn :: s -> s
  showGame :: s -> Text
  parseGame :: Parser s
  endTurn = resolveDeaths . resolveSlime . resolveUnits . resolveOrders
  showGame s = T.unlines [printNodes s, printEnds s,
    printSlime s, printUnits s, printOrders s]

