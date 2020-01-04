{-# LANGUAGE OverloadedStrings #-}

module Parse.Interact
  ( Command (..)
  , command
  ) where

import Engine

import           Control.Monad.Identity     (Identity)
import           Data.Char                  (isDigit, isSpace)
import           Data.Text                  (Text, unpack)
import           Data.Type.Nat              (SNatI)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

data Command n =
    Save FilePath -- Save the game
  | Load FilePath -- Load a game
  | Quit -- Quit the game
  | Move (NodeID n) (NodeID n) -- Give a command
  | Clear (NodeID n) -- Clear orders from Node
  | Show -- Show the board
  | Peek -- Show current resolution
  | Turn -- End turn
  | Status -- Show victory status
  | Graph -- Show graphViz graph
  deriving Show

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

node :: SNatI n => Parser (NodeID n)
node = lexeme $ fromInteger . read . unpack <$> takeWhile1P (Just "NodeID") isDigit

nodePair :: SNatI n => (NodeID n -> NodeID n -> b) -> Parser b
nodePair c = do
  n1 <- node
  n2 <- node
  eof
  return . c n1 $ n2

file :: Parser FilePath
file = unpack <$> (singleQuoted <|> doubleQuoted <|> unquoted)
  where
    singleQuoted = singleQuotes (takeWhile1P (Just "filepath") (/= '\''))
    doubleQuoted = doubleQuotes (takeWhile1P (Just "filepath") (/= '"'))
    unquoted = takeWhile1P (Just "filepath") (not . isSpace)

fileOp :: (FilePath -> b) -> Parser b
fileOp op = do
  f <- lexeme file
  eof
  return . op $ f

command :: SNatI n => Parser (Command n)
command = do
  hidden space
  choice
    [ symbol "quit" >> eof >> return Quit
    , symbol "show" >> eof >> return Show
    , symbol "peek" >> eof >> return Peek
    , symbol "turn" >> eof >> return Turn
    , symbol "status" >> eof >> return Status
    , symbol "graph" >> eof >> return Graph
    , symbol "clear" >> do
      n <- node
      eof
      return $ Clear n
    , symbol "move" >> nodePair Move
    , symbol "save" >> fileOp Save
    , symbol "load" >> fileOp Load ]
