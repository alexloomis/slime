{-# LANGUAGE OverloadedStrings #-}

module Interface
  ( loop
  ) where

import Engine
import GameState
import Parser.Ingame

import qualified Data.Text.IO     as T
import           GHC.IO.Handle.FD (stdin)
import           Text.Megaparsec  (errorBundlePretty, runParser)

move :: GameState s => Order -> s -> IO s
move newOrder@(Order n1 _) s = case checkOrder newOrder s of
  -- If everything works, add order.
  Right order -> do
    T.putStrLn "Order added."
    return $ giveOrder order s
  Left err -> case err of
    -- If the error is "already has order", try overwriting.
    (-4, _) -> case checkOrder newOrder (clearOrderFrom n1 s) of
      Right order -> do
        T.putStrLn "Previous order overwritten."
        return $ giveOrder order (clearOrderFrom n1 s)
      Left (_, msg) -> giveMsg msg
    (_, msg) -> giveMsg msg
  where
    giveMsg m = T.putStrLn m >> return s

clear :: GameState s => Node -> s -> IO s
clear n s = T.putStrLn "Order cleared." >> return (clearOrderFrom n s)

save :: GameState s => FilePath -> s -> IO s
save fp s = T.writeFile fp (showGame s) >> T.putStrLn "File saved." >> return s

load :: GameState s => FilePath -> s -> IO s
load fp s = do
  input <- T.readFile fp
  case runParser parseGame fp input of
    Left err -> do
      putStrLn . errorBundlePretty $ err
      return s
    Right new -> T.putStrLn "File loaded." >> return new

quit :: GameState s => s -> IO s
quit s = T.putStrLn "Bye!" >> return s

showS :: GameState s => s -> IO s
showS s = T.putStrLn (showGame s) >> return s

peek :: GameState s => s -> IO s
peek s = T.putStrLn (showGame (endTurn s)) >> return s

done :: GameState s => s -> IO s
done s = T.putStrLn "Ending turn." >> return (endTurn s)

loop :: GameState s => s -> IO s
loop s = do
  input <- T.hGetLine stdin
  case runParser command "" input of
    Left err -> do
      putStrLn . errorBundlePretty $ err
      loop s
    Right cmd -> case cmd of
      Move n1 n2 -> move (Order n1 n2) s >>= loop
      Clear n    -> clear n s >>= loop
      Save fp    -> save fp s >>= loop
      Load fp    -> load fp s >>= loop
      Show       -> showS s >>= loop
      Peek       -> peek s >>= loop
      Done       -> done s >>= loop
      Quit       -> quit s

