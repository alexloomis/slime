{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Board where

import Engine
import GameState
import Internal.Print
import Parse.Save

import           Control.Lens.Combinators
import           Data.Maybe               (fromJust, isNothing)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Type.Nat            (SNatI)
import           Data.Vec.Lazy            (Vec, fromListPrefix, toList)
import           GHC.Generics             (Generic)

newtype NodeName = NodeName {_nodeName :: Text} deriving (Eq, Generic)

data Board n = Board
  { _ends   :: Vec n (Ends n)
  , _slime  :: Vec n Slime
  , _units  :: Vec n (Maybe Unit)
  , _orders :: Vec n (Order n) }
  deriving (Eq, Generic)

$(makeFieldsNoPrefix ''Board)

makeBoard :: Vec n (Ends n) -> Vec n Slime -> Vec n (Maybe Unit)
  -> [OneOrder n] -> Board n
makeBoard _ends _slime _units = foldr giveOrder
  (Board
    { _ends
    , _slime
    , _units
    , _orders = fmap (const $ Order Nothing) _ends })

allN :: SNatI n => a -> Vec n a
allN = fromJust . fromListPrefix . repeat

emptyBoard :: SNatI n => Board n
emptyBoard = Board
  (allN . allN $ 0)
  (allN 0)
  (allN Nothing)
  (allN $ Order Nothing)

updateField :: Parsed n -> Board n -> Board n
updateField = \case
  ParsedEnds p -> set ends p
  ParsedSlime p -> set slime p
  ParsedUnits p -> set units p
  ParsedOrders p -> set orders p

parseBoard :: SNatI n => Parser (Board n)
parseBoard = foldr updateField emptyBoard <$> parseSave

instance SNatI n => GameState (Board n) n where
  victory s
    | all (== 0) (toList . getSlime $ s) = Win
    | all isNothing (toList . getUnits $ s) = Lose
    | otherwise = Ongoing
  showGame s = T.unlines
    [printEnds s, printSlime s, printUnits s, printOrders s]
  parseGame = parseBoard

instance SNatI n => Show (Board n) where
  show = show . showGame

