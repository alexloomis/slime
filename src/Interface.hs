{-# LANGUAGE OverloadedStrings #-}

module Interface
  (
  loop
  ) where

import Board
import Engine
import Engine.Print
import Parser.Ingame
import Parser.Save

import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           GHC.IO.Handle.FD (stdin)
import           Text.Megaparsec  (errorBundlePretty, runParser)

endTurn = resolveDeaths . resolveSlime . resolveUnits . resolveOrders
showState s = T.unlines [printNodes s, printEnds s,
  printSlime s, printUnits s, printOrders s]
prettyState = printBoard
  . renameNodes (\(Node t) -> Node $ "N: " `T.append` (T.pack . show $ t))

move :: Order -> Board -> IO Board
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

clear :: Node -> Board -> IO Board
clear n s = T.putStrLn "Order cleared." >> return (clearOrderFrom n s)

save :: FilePath -> Board -> IO Board
save fp s = T.writeFile fp (showState s) >> T.putStrLn "File saved." >> return s

load :: FilePath -> Board -> IO Board
load fp s = do
  input <- T.readFile fp
  case runParser parseSave fp input of
    Left err -> do
      putStrLn . errorBundlePretty $ err
      return s
    Right new -> T.putStrLn "File loaded." >> return new

quit :: Board -> IO Board
quit s = T.putStrLn "Bye!" >> return s

showS :: Board -> IO Board
showS s = T.putStrLn (showState s) >> return s

peek :: Board -> IO Board
peek s = T.putStrLn (showState (endTurn s)) >> return s

done :: Board -> IO Board
done s = T.putStrLn "Ending turn." >> return (endTurn s)

loop :: Board -> IO Board
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

