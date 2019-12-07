{-# LANGUAGE OverloadedStrings #-}
module Board.Printer where

import           Board
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text         as T



{-
-- $ show (iden node) ++ " [label = \"" ++ show (sand node) ++ "\"];"
printNodes :: NodeMap -> [T.Text]
printNodes = fmap T.pack . HM.elems . HM.mapWithKey pretty . getMap
  where pretty iden sand = show iden ++ " [label = \"" ++ show sand ++ "\"];"

printEdge :: Edge -> T.Text
printEdge edge = T.pack
  $ show (getFrom edge) ++ " -> " ++ show (getTo edge) ++ ";"

indent :: T.Text -> T.Text
indent = T.append "  "

prePrintGraph :: Sandpile -> [T.Text]
prePrintGraph pile = printNodes (getNodeMap pile)
  ++ fmap printEdge (getEdges pile)

printGraph :: Sandpile -> T.Text
printGraph pile = T.unlines
  $ ["digraph Sandpile {"]
  ++ fmap indent (prePrintGraph pile)
  ++ ["}"]
-}
