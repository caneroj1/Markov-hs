module Lib
    ( empty
    , insert
    , getChainFrom
    , genFromNGrams
    , MarkovTree
    ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, alter, toList, null, lookup)
import Data.List (foldl')
import System.Random

type Count      = Int
type Label      = Text
type TextToNode = Map Label MarkovNode

data MarkovNode = MarkovNode {
  count      :: Count
, label      :: Label
, textToNode :: TextToNode } deriving Show

data MarkovTree = MarkovTree TextToNode deriving Show

empty :: MarkovTree
empty = MarkovTree Map.empty

insert :: MarkovTree -> [Label] -> MarkovTree
insert tree                    []     = tree
insert (MarkovTree textToNode) labels =
  MarkovTree (insertLabels textToNode labels)

genFromNGrams :: [[Label]] -> MarkovTree
genFromNGrams = foldl' insert empty

getChainFrom :: MarkovTree -> Int -> StdGen -> [Label]
getChainFrom (MarkovTree tree) chainLength gen
  | chainLength <= 0    = []
  | otherwise           = go tree gen chainLength
  where
    go m g 0 = []
    go m g n = label randomNode : continue randomNode g'
      where
        (randomNode, g') = getRandomFrom g m
        continue rn gen
          | Map.null (textToNode rn) =
            case Map.lookup (label rn) tree of
              Just node  -> go (textToNode node) gen (n-1)
              Nothing    -> []
          | otherwise                = go (textToNode rn) gen (n-1)

-- Utility --
insertLabels :: TextToNode -> [Label] -> TextToNode
insertLabels textToNode []     = textToNode
insertLabels textToNode (l:ls) = Map.alter incOrIns l textToNode
  where
    incOrIns Nothing = Just (MarkovNode 1 l $ insertLabels Map.empty ls)
    incOrIns (Just (MarkovNode c l m)) =
      Just (MarkovNode (c+1) l $ insertLabels m ls)

getRandomFrom :: StdGen -> TextToNode -> (MarkovNode, StdGen)
getRandomFrom gen textToNode = getItem randomIdx list
  where
    list            = map snd $ Map.toList textToNode
    sumOfWeights    = sum $ map count list
    (randomIdx, g') = randomR (1, sumOfWeights) gen
    getItem 0 (n:_)      = (n, g')
    getItem w (n:ns)
      | w - count n <= 0 = (n, g')
      | otherwise        = getItem (w - count n) ns
