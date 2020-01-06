{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Board where

import Prelude hiding (replicate)

import Engine
import GameState
import Internal.Print
import Parse.Save

import           Control.Lens.Combinators
import           Data.Maybe               (isNothing)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Vector.Sized        (Vector, replicate, toList)
import           GHC.Generics             (Generic)
import           GHC.TypeNats             (KnownNat)

newtype NodeName = NodeName {_nodeName :: Text} deriving (Eq, Generic)

data Board n = Board
  { _ends   :: Vector n (Ends n)
  , _slime  :: Vector n Slime
  , _units  :: Vector n (Maybe Unit)
  , _orders :: Vector n (Order n) }
  deriving (Eq, Generic)

$(makeFieldsNoPrefix ''Board)

makeBoard :: Vector n (Ends n) -> Vector n Slime -> Vector n (Maybe Unit)
  -> [OneOrder n] -> Board n
makeBoard _ends _slime _units = foldr giveOrder
  (Board
    { _ends
    , _slime
    , _units
    , _orders = fmap (const $ Order Nothing) _ends })

emptyBoard :: KnownNat n => Board n
emptyBoard = Board
  (replicate . replicate $ 0)
  (replicate 0)
  (replicate Nothing)
  (replicate $ Order Nothing)

updateField :: Parsed n -> Board n -> Board n
updateField = \case
  ParsedEnds p -> set ends p
  ParsedSlime p -> set slime p
  ParsedUnits p -> set units p
  ParsedOrders p -> set orders p

parseBoard :: KnownNat n => Parser (Board n)
parseBoard = foldr updateField emptyBoard <$> parseSave

instance KnownNat n => GameState (Board n) n where
  victory s
    | all (== 0) (toList . getSlime $ s) = Win
    | all isNothing (toList . getUnits $ s) = Lose
    | otherwise = Ongoing
  showGame s = T.unlines
    [printEnds s, printSlime s, printUnits s, printOrders s]
  parseGame = parseBoard

instance KnownNat n => Show (Board n) where
  show = T.unpack . showGame

