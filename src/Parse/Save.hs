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
import Data.Type.Nat        (Nat (..), SNatI)
import Data.Vec.Lazy        (Vec (..))
import Text.Megaparsec
import Text.Megaparsec.Char

data Parsed n =
    ParsedEnds (Vec n (Ends n))
  | ParsedSlime (Vec n Slime)
  | ParsedUnits (Vec n (Maybe Unit))
  | ParsedOrders (Vec n (Order n))

data Heading = ENDS | SLIME | UNITS | ORDERS

heading :: Parser Heading
heading = lexeme $ choice
  [ symbol "[ENDS]" >> return ENDS
  , symbol "[SLIME]" >> return SLIME
  , symbol "[UNITS]" >> return UNITS
  , symbol "[ORDERS]" >> return ORDERS ]

node :: SNatI n => Parser (NodeID n)
node = integerLike "NodeID"

weight :: Parser Nat
weight = integerLike "Weight"

slime :: Parser Slime
slime = Slime <$> integerLike "Slime"

unit :: Parser Unit
unit = lexeme $ choice
  [ symbol "Sprayer" >> return Sprayer
  , symbol "Lobber" >> return Lobber ]

order :: SNatI n => Parser (Order n)
order = Order . Just <$> integerLike "Order"

edgeWeight :: SNatI n => Parser (NodeID n, Nat)
edgeWeight = do
  notFollowedBy (anInteger >> symbol "->")
  m <- node
  _ <- symbol "#"
  n <- weight
  return (m,n)

edgeWeights :: SNatI n => Parser (Ends n)
edgeWeights = do
  ls <- many edgeWeight
  lookAhead (eof <|> void heading <|> void edgeFrom)
  return . fromMap 0 $ ls
  where edgeFrom = anInteger >> symbol "->"

arrowLine :: SNatI n => Parser a -> Parser (NodeID n, a)
arrowLine p = do
  x <- node
  _ <- symbol "->"
  y <- p
  return (x,y)

parseVec :: SNatI n => v -> Parser (NodeID n, v) -> Parser (Vec n v)
parseVec d p = do
  ls <- many (lexeme p)
  lookAhead (eof <|> void heading)
  return . fromMap d $ ls

chooseParser :: SNatI n => Heading -> Parser (Parsed n)
chooseParser = \case
  ENDS -> ParsedEnds <$> parseVec (fromMap 0 []) (arrowLine edgeWeights)
  SLIME -> ParsedSlime <$> parseVec 0 (arrowLine slime)
  UNITS -> ParsedUnits <$> parseVec Nothing (fmap Just <$> arrowLine unit)
  ORDERS -> ParsedOrders <$> parseVec (Order Nothing) (arrowLine order)

parseSave :: SNatI n => Parser [Parsed n]
parseSave = do
  hidden space
  p <- many (heading >>= chooseParser)
  eof
  return p

