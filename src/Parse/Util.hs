{-# LANGUAGE OverloadedStrings #-}

module Parse.Util where

import           Control.Monad.Identity     (Identity)
import           Data.Char                  (isDigit)
import           Data.Text                  (Text, unpack)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

readT :: Read a => Text -> a
readT = read . unpack

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

-- |Returns bracketed value.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

anInteger :: Parser Integer
anInteger = lexeme $ readT <$> takeWhile1P (Just "An Integer") isDigit

integerLike :: Num a => String -> Parser a
integerLike x = fromInteger <$> anInteger <?> x

