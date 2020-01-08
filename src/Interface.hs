{-# LANGUAGE OverloadedStrings #-}

module Interface
  ( loop
  ) where

import Engine
import GameState
import Parse.Interact

import qualified Data.Text.IO     as T
import           GHC.IO.Handle.FD (stdin)
import           Text.Megaparsec  (errorBundlePretty, runParser)

move :: GameState s n => OneOrder n -> s -> IO s
move newOrder@(OneOrder n1 _) s = case checkOrder newOrder s of
  -- If everything works, add order.
  Right order -> do
    T.putStrLn "Order added."
    return $ giveOrder order s
  Left err -> case err of
    -- If the error is "already has order", try overwriting.
    (AlreadyCmd, _) -> case checkOrder newOrder (clearOrderFrom n1 s) of
      Right order -> do
        T.putStrLn "Previous order overwritten."
        return $ giveOrder order (clearOrderFrom n1 s)
      Left (_, msg) -> giveMsg msg
    (_, msg) -> giveMsg msg
  where
    giveMsg m = T.putStrLn m >> return s

clear :: GameState s n => NodeID n -> s -> IO s
clear n s = T.putStrLn "Order cleared." >> return (clearOrderFrom n s)

save :: GameState s n => FilePath -> s -> IO s
save fp s = T.writeFile fp (showGame s) >> T.putStrLn "File saved." >> return s

load :: GameState s n => FilePath -> s -> IO s
load fp s = do
  input <- T.readFile fp
  case runParser parseGame fp input of
    Left err -> do
      putStrLn . errorBundlePretty $ err
      return s
    Right new -> T.putStrLn "File loaded." >> return new

quit :: IO ()
quit = T.putStrLn "Bye!"

showS :: GameState s n => s -> IO s
showS s = T.putStrLn (showGame s) >> return s

peek :: GameState s n => s -> IO s
peek s = T.putStrLn (showGame (endTurn s)) >> return s

turn :: GameState s n => s -> IO s
turn s = T.putStrLn "Ending turn." >> return (endTurn s)

status :: GameState s n => s -> IO s
status s = (print . victory $ s) >> return s

graph :: GameState s n => s -> IO s
graph s = T.putStrLn (graphGame s) >> return s

loop :: GameState s n => s -> IO ()
loop s = do
  input <- T.hGetLine stdin
  case runParser command "" input of
    Left err -> do
      putStrLn . errorBundlePretty $ err
      loop s
    Right cmd -> case cmd of
      Move n1 n2 -> move (OneOrder n1 n2) s >>= loop
      Clear n    -> clear n s >>= loop
      Save fp    -> save fp s >>= loop
      Load fp    -> load fp s >>= loop
      Show       -> showS s >>= loop
      Peek       -> peek s >>= loop
      Turn       -> turn s >>= loop
      Status     -> status s >>= loop
      Graph      -> graph s >>= loop
      Quit       -> quit

