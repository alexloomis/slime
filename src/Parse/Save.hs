{-# LANGUAGE OverloadedStrings #-}

module Parse.Save
  ( Parser
  , Parsed (..)
  , parseSave
  ) where

import Engine        hiding (slime)
import Internal.Util (fromMap)
import Parse.Util

import Control.Monad        (void)
import Data.Vector.Sized    (Vector)
import GHC.TypeNats         (KnownNat)
import Numeric.Natural      (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char

data Parsed n =
    ParsedEnds (Vector n (Ends n))
  | ParsedSlime (Vector n Slime)
  | ParsedUnits (Vector n (Maybe Unit))
  | ParsedOrders (Vector n (Order n))

data Heading = ENDS | SLIME | UNITS | ORDERS

heading :: Parser Heading
heading = lexeme $ choice
  [ symbol "[ENDS]" >> return ENDS
  , symbol "[SLIME]" >> return SLIME
  , symbol "[UNITS]" >> return UNITS
  , symbol "[ORDERS]" >> return ORDERS ]

node :: KnownNat n => Parser (NodeID n)
node = integerLike "NodeID"

weight :: Parser Natural
weight = integerLike "Weight"

slime :: Parser Slime
slime = Slime <$> integerLike "Slime"

unit :: Parser Unit
unit = lexeme $ choice
  [ symbol "Sprayer" >> return Sprayer
  , symbol "Lobber" >> return Lobber ]

order :: KnownNat n => Parser (Order n)
order = Order . Just <$> integerLike "Order"

edgeWeight :: KnownNat n => Parser (NodeID n, Natural)
edgeWeight = do
  notFollowedBy (anInteger >> symbol "->")
  m <- node
  _ <- symbol "#"
  n <- weight
  return (m,n)

edgeWeights :: KnownNat n => Parser (Ends n)
edgeWeights = do
  ls <- many edgeWeight
  lookAhead (eof <|> void heading <|> void edgeFrom)
  return . fromMap 0 $ ls
  where edgeFrom = anInteger >> symbol "->"

arrowLine :: KnownNat n => Parser a -> Parser (NodeID n, a)
arrowLine p = do
  x <- node
  _ <- symbol "->"
  y <- p
  return (x,y)

parseVector :: KnownNat n => v -> Parser (NodeID n, v) -> Parser (Vector n v)
parseVector d p = do
  ls <- many (lexeme p)
  lookAhead (eof <|> void heading)
  return . fromMap d $ ls

chooseParser :: KnownNat n => Heading -> Parser (Parsed n)
chooseParser = \case
  ENDS -> ParsedEnds <$> parseVector (fromMap 0 []) (arrowLine edgeWeights)
  SLIME -> ParsedSlime <$> parseVector 0 (arrowLine slime)
  UNITS -> ParsedUnits <$> parseVector Nothing (fmap Just <$> arrowLine unit)
  ORDERS -> ParsedOrders <$> parseVector (Order Nothing) (arrowLine order)

parseSave :: KnownNat n => Parser [Parsed n]
parseSave = do
  hidden space
  p <- many (heading >>= chooseParser)
  eof
  return p

