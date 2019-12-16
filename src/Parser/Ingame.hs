{-# LANGUAGE OverloadedStrings #-}

module Parser.Ingame
  ( Command (..)
  , command
  ) where

import Engine

import           Control.Monad              (void)
import           Control.Monad.Identity     (Identity)
import           Data.Char                  (isAlphaNum, isSpace)
import           Data.Text                  (Text, append, pack, unpack)
import           Data.Void                  (Void)
import           System.FilePath
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

data Command =
    Save (Maybe FilePath) -- Save the game
  | Load (Maybe FilePath) -- Load a game
  | Quit -- Quit the game
  | Move Node Node -- Give a command
  | Clear Node -- Clear orders from Node
  | Show -- Show the board
  | Peek -- Show current resolution
  | Done -- End turn
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

node :: Parser Node
node = Node <$> (singleQuoted <|> doubleQuoted <|> unquoted)
  where
    singleQuoted = singleQuotes (takeWhile1P (Just "Node") (/= '\''))
    doubleQuoted = doubleQuotes (takeWhile1P (Just "Node") (/= '"'))
    unquoted = takeWhile1P (Just "Node") isAlphaNum

nodePair :: (Node -> Node -> b) -> Parser b
nodePair c = do
  n1 <- lexeme node
  n2 <- lexeme node
  eof
  return . c n1 $ n2

optFile :: Parser (Maybe FilePath)
optFile = optional $ unpack <$> (singleQuoted <|> doubleQuoted <|> unquoted)
  where
    singleQuoted = singleQuotes (takeWhile1P (Just "File Path") (/= '\''))
    doubleQuoted = doubleQuotes (takeWhile1P (Just "File Path") (/= '"'))
    unquoted = takeWhile1P (Just "File Path") (not . isSpace)

fileOp :: (Maybe FilePath -> b) -> Parser b
fileOp op = do
  file <- lexeme optFile
  eof
  return . op $ file

command :: Parser Command
command = do
  hidden space
  choice
    [ symbol "quit" >> eof >> return Quit
    , symbol "show" >> eof >> return Show
    , symbol "done" >> eof >> return Done
    , symbol "clear" >> do
      n <- lexeme node
      eof
      return $ Clear n
    , symbol "move" >> nodePair Move
    , symbol "save" >> fileOp Save
    , symbol "load" >> fileOp Load ]

