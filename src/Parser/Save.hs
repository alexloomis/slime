{-# LANGUAGE OverloadedStrings #-}

module Parser.Save
  ( parseSave
  ) where

import Engine
import Engine.Util.MaybeNat

import           Control.Monad              (void)
import           Control.Monad.Identity     (Identity)
import           Data.Char                  (isAlphaNum, isDigit)
import qualified Data.HashMap.Lazy          as HM
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS
import           Data.Text                  (Text, unpack)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

-- data Heading = HNodes | HEnds | HSlime | HUnits | HOrders

-- |Eats nonzero amount of whitespace.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- |Returns quoted value.
singleQuotes :: Parser a -> Parser a
singleQuotes = between (char '\'') (symbol "'")

-- |Returns quoted value.
doubleQuotes :: Parser a -> Parser a
doubleQuotes = between (char '"') (symbol "\"")

heading :: Parser ()
heading = choice $ fmap (void . symbol)
  [ "[NODES]"
  , "[EDGES]"
  , "[SLIME]"
  , "[UNITS]"
  , "[ORDERS]" ]

node :: Parser Node
node = lexeme $ Node <$> (singleQuoted <|> doubleQuoted <|> unquoted)
  where
    singleQuoted = singleQuotes (takeWhile1P (Just "node") (/= '\''))
    doubleQuoted = doubleQuotes (takeWhile1P (Just "node") (/= '"'))
    unquoted = takeWhile1P (Just "node") isAlphaNum

weight :: Parser Int
weight = lexeme $ read . unpack <$> takeWhile1P (Just "weight") isDigit

slime :: Parser Slime
slime = lexeme $ Slime . MNat . read . unpack
  <$> takeWhile1P (Just "slime") isDigit

unit :: Parser Unit
unit = lexeme $ choice
  [ symbol "Sprayer" >> return Sprayer
  , symbol "Lobber" >> return Lobber ]

edgeWeight :: Parser (Node, Int)
edgeWeight = do
  notFollowedBy (node >> symbol "->")
  n <- node
  m <- optional $ do
    _ <- symbol "#"
    weight
  return $ case m of
    Nothing -> (n, 1)
    Just k  -> (n, k)

edgeTo :: Parser Ends
edgeTo = do
  ls <- many edgeWeight
  lookAhead (eof <|> heading <|> void edgeFrom)
  return . HM.fromList $ ls
  where
    edgeFrom = node >> symbol "->"

parseEndLine :: Parser (Node, Ends)
parseEndLine = do
  n <- node
  _ <- symbol "->"
  e <- edgeTo
  return (n,e)

parseSlimeLine :: Parser (Node, Slime)
parseSlimeLine = do
  n <- node
  _ <- symbol "->"
  s <- slime
  return (n,s)

parseUnitLine :: Parser (Node, Maybe Unit)
parseUnitLine = do
  n <- node
  _ <- symbol "->"
  u <- unit
  return (n, Just u)

parseOrderLine :: Parser (Node, Maybe Node)
parseOrderLine = do
  n1 <- node
  _ <- symbol "->"
  n2 <- node
  return (n1, Just n2)

parseNodes :: Parser (HashSet Node)
parseNodes = do
  ns <- many node
  lookAhead (eof <|> heading)
  return . HS.fromList $ ns

parseAttr :: Parser (Node, v) -> Parser (NodeAttr v)
parseAttr p = do
  ls <- many (lexeme p)
  lookAhead (eof <|> heading)
  return . HM.fromList $ ls

parseEnds :: Parser (NodeAttr Ends)
parseEnds = do
  ls <- many parseEndLine
  lookAhead (eof <|> heading)
  return . HM.fromList $ ls

parseSlime :: Parser (NodeAttr Slime)
parseSlime = parseAttr parseSlimeLine

parseUnits :: Parser (NodeAttr (Maybe Unit))
parseUnits = parseAttr parseUnitLine

parseOrders :: Parser (NodeAttr (Maybe Node))
parseOrders = parseAttr parseOrderLine

-- |Currently requires Nodes, Edges, Slime, Units, Orders, in order.
parseSave :: Parser
  (HashSet Node, NodeAttr Ends, NodeAttr Slime,
  NodeAttr (Maybe Unit), NodeAttr (Maybe Node))
parseSave = do
  hidden space
  heading
  ns <- parseNodes
  heading
  es <- parseEnds
  heading
  sl <- parseSlime
  heading
  us <- parseUnits
  heading
  os <- parseOrders
  eof
  return (ns, es, sl, us, os)

--------------------------
-- For testing
--------------------------

{-
tnodes :: Text
tnodes = "Cherry\nDurian\nBlueberry\nFig\nApple\n\n"

tends :: Text
tends = "Cherry ->\n  Cherry\nBlueberry ->\n  Fig\nFig ->\n  Cherry\n  Blueberry\n  Apple # 2\nApple ->\n  Blueberry\n\n"

tslime :: Text
tslime = "Cherry -> 2\nDurian -> 0\nBlueberry -> 3\nFig -> 17\nApple -> 7\n\n"

tunits :: Text
tunits = "Cherry -> Sprayer\nApple -> Lobber\n\n"

torders :: Text
torders = "Apple -> Blueberry\n\n"
-}

