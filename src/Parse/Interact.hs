{-# LANGUAGE OverloadedStrings #-}

module Parse.Interact
  ( Command (..)
  , command
  ) where

import Engine
import Parse.Util

import Data.Char            (isSpace)
import Data.Text            (unpack)
import GHC.TypeNats         (KnownNat)
import Text.Megaparsec
import Text.Megaparsec.Char

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

node :: KnownNat n => Parser (NodeID n)
node = integerLike "NodeID"

nodePair :: KnownNat n => (NodeID n -> NodeID n -> b) -> Parser b
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

command :: KnownNat n => Parser (Command n)
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

