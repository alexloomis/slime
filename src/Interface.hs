{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interface
  ( loop
  , parseThenDo
  , doList
  ) where

import Engine
import GameState
import Parse.Interact

import           Data.Text        (Text)
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

-- Does not handle Quit
parseThenDo :: GameState s n => s -> Text -> IO s
parseThenDo s t =
  case runParser command "" t of
    Left err -> (putStrLn . errorBundlePretty $ err) >> return s
    Right cmd -> case cmd of
      Move n1 n2 -> move (OneOrder n1 n2) s
      Clear n    -> clear n s
      Save fp    -> save fp s
      Load fp    -> load fp s
      Show       -> showS s
      Peek       -> peek s
      Turn       -> turn s
      Status     -> status s
      Graph      -> graph s
      Quit       -> putStrLn "Invalid command 'quit'." >> return s

loop :: forall s n. GameState s n => s -> IO ()
loop s = do
  t <- T.hGetLine stdin
  case runParser command "" t of
    Right (Quit :: Command n) -> return ()
    _                         -> parseThenDo s t >>= loop

---------- Tools for commands from files/input args

doList :: GameState s n => [Text] -> IO s -> IO s
doList []     = id
doList (x:xs) = (=<<) (doList xs . flip parseThenDo x)

