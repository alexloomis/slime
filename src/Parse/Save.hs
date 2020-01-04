{-# LANGUAGE OverloadedStrings #-}

module Parse.Save
  ( Parser
  , Parsed (..)
  , parseSave
  ) where

import Engine        hiding (slime)
import Internal.Util (fromMap)

import           Control.Monad              (void)
import           Control.Monad.Identity     (Identity)
import           Data.Char                  (isDigit)
import           Data.Text                  (Text, unpack)
import           Data.Type.Nat              (Nat (..), SNatI)
import           Data.Vec.Lazy              (Vec (..))
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

data Parsed n =
    ParsedEnds (Vec n (Ends n))
  | ParsedSlime (Vec n Slime)
  | ParsedUnits (Vec n (Maybe Unit))
  | ParsedOrders (Vec n (Order n))

data Heading = ENDS | SLIME | UNITS | ORDERS

readT :: Read a => Text -> a
readT = read . unpack

-- |Eats nonzero amount of whitespace.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

heading :: Parser Heading
heading = lexeme $ choice
  [ symbol "[ENDS]" >> return ENDS
  , symbol "[SLIME]" >> return SLIME
  , symbol "[UNITS]" >> return UNITS
  , symbol "[ORDERS]" >> return ORDERS ]

anInteger :: Parser Integer
anInteger = lexeme $ readT <$> takeWhile1P (Just "An Integer") isDigit

integerLike :: Num a => String -> Parser a
integerLike x = fromInteger <$> anInteger <?> x

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
  -- notFollowedBy (node >> symbol "->")
  m <- node
  _ <- symbol "#"
  n <- weight
  return (m,n)

edgeWeights :: SNatI n => Parser (Ends n)
edgeWeights = do
  ls <- many edgeWeight
  lookAhead (eof <|> void heading <|> void edgeFrom)
  return . fromMap $ ls
  where edgeFrom = anInteger >> symbol "->"

arrowLine :: SNatI n => Parser a -> Parser (NodeID n, a)
arrowLine p = do
  x <- node
  _ <- symbol "->"
  y <- p
  return (x,y)

parseEndLine :: SNatI n => Parser (NodeID n, Ends n)
parseEndLine = arrowLine edgeWeights

parseSlimeLine :: SNatI n => Parser (NodeID n, Slime)
parseSlimeLine = arrowLine slime

parseUnitLine :: SNatI n => Parser (NodeID n, Maybe Unit)
parseUnitLine = fmap Just <$> arrowLine unit

parseOrderLine :: SNatI n => Parser (NodeID n, Order n)
parseOrderLine = arrowLine order

parseVec :: SNatI n => Parser (NodeID n, v) -> Parser (Vec n v)
parseVec p = do
  ls <- many (lexeme p)
  lookAhead (eof <|> void heading)
  return . fromMap $ ls

chooseParser :: SNatI n => Heading -> Parser (Parsed n)
chooseParser = \case
  ENDS -> ParsedEnds <$> parseVec parseEndLine
  SLIME -> ParsedSlime <$> parseVec parseSlimeLine
  UNITS -> ParsedUnits <$> parseVec parseUnitLine
  ORDERS -> ParsedOrders <$> parseVec parseOrderLine

parseSave :: SNatI n => Parser [Parsed n]
parseSave = do
  hidden space
  p <- many (heading >>= chooseParser)
  eof
  return p

