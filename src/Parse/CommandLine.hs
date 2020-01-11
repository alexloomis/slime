{-# LANGUAGE DuplicateRecordFields #-}

module Parse.CommandLine
  ( Commands (..)
  , Options (..)
  , opts
  ) where

import Data.Text           (Text, pack)
import Numeric.Natural     (Natural)
import Options.Applicative

data Commands = FromFile FilePath | FromTerm [Text]

data Options = Options
  { boardSize :: Maybe Natural
  , noQuit    :: Bool
  , commands  :: Maybe Commands }

fpCommand :: Parser Commands
fpCommand = FromFile <$> strOption
  (  long "input"
  <> short 'i'
  <> help "Input file"
  <> metavar "FILE" )

ftCommand :: Parser Commands
ftCommand = FromTerm <$> some (pack <$> strOption
  (  long "command"
  <> short 'c'
  <> help "Command to be run" ))

commandParser :: Parser Commands
commandParser = fpCommand <|> ftCommand

options :: Parser Options
options = Options
  <$> optional (option auto
    (  long "size"
    <> short 's'
    <> help "Board size"
    <> metavar "NAT" ))
  <*> switch
    (  long "noquit"
    <> short 'n'
    <> showDefault
    <> help "Stay open after reading commands" )
  <*> optional commandParser

opts :: ParserInfo Options
opts = info (options <**> helper)
  (  fullDesc
  <> progDesc "Program description"
  <> header "Program header" )

