{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Board
import Interface
import Parse.CommandLine

import           Control.Monad       (void)
import           Data.Maybe          (fromJust)
import           Data.Proxy          (Proxy)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           GHC.TypeNats        (KnownNat, SomeNat (..), someNatVal)
import           Numeric.Natural     (Natural)
import           Options.Applicative (execParser)
import           Text.Read           (readMaybe)

getNat :: IO Natural
getNat = do
  putStrLn "Board size?"
  input <- getLine
  let parsed = readMaybe input :: Maybe Integer
  if parsed >= Just 0
    then return . fromInteger . fromJust $ parsed
    else do
      putStrLn "Invalid input, defaulting to zero."
      return 0

getCmds :: Commands -> IO [Text]
getCmds (FromFile fp) = filter (/= "") . T.lines <$> T.readFile fp
getCmds (FromTerm cs) = pure cs

runCmds :: KnownNat n => [Text] -> IO (Board n)
runCmds cs = doList cs (pure emptyBoard)

runWith :: [Text] -> Natural -> IO ()
runWith cs x = case someNatVal x of
  SomeNat (_ :: Proxy n) -> do
    s <- runCmds cs
    loop (s :: Board n)

runWith' :: [Text] -> Natural -> IO ()
runWith' cs x = case someNatVal x of
  SomeNat (_ :: Proxy n) -> void (runCmds cs :: IO (Board n))

run :: Options -> Natural -> IO ()
run Options{commands = Nothing} n = runWith [] n
run Options{commands = Just cs, noQuit = p} n = do
  cmds <- getCmds cs
  (if p then runWith else runWith') cmds n

main :: IO ()
main = do
  options <- execParser opts
  nat <- case boardSize options of
    Nothing -> getNat
    Just n  -> pure n
  run options nat

