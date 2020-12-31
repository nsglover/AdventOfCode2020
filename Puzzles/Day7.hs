module Puzzles.Day7 (Day) where

import Common.Solution
import Common.Parsing
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

data Day = Day
instance Solve Day where
  part1 _ ls = show $ countContainment "shiny gold" (map (toSimpleRule . parse) ls) - 1
  part2 _ ls = show $ countContents "shiny gold" (Map.fromList (map parse ls)) - 1

type BagColor = String
type BagRule = (BagColor, [(BagColor, Int)])

parse :: String -> BagRule
parse s = 
  let 
    s' = splitOn ' ' s 
    container = head s' ++ " " ++ (s' !! 1)
    s'' = splitOn ',' (foldl1 (\s1 s2 -> s1 ++ " " ++ s2) (drop 4 s'))
    contents = if head s'' == "no other bags." then [] else
      let
        toPair ss = ((ss !! 1) ++ " " ++ (ss !! 2), read (head ss))
        attr = map (toPair . splitOn ' ') s''
      in attr
  in (container, contents)

toSimpleRule :: BagRule -> (BagColor, Set BagColor)
toSimpleRule (container, contents) = (container, Set.fromList (map (\(c,_) -> c) contents))

countContainment :: BagColor -> [(BagColor, Set BagColor)] -> Int
countContainment c s = 
  let
    helper set [] pile oldSize = let n = Set.size set in if n == oldSize then n else helper set pile [] n
    helper set q@(rule@(color, contents):xs) pile oldSize =
      if Set.null (Set.intersection set contents) then helper set xs (rule:pile) oldSize
      else helper (Set.insert color set) xs pile oldSize
  in helper (Set.singleton c) s [] 0

countContents :: BagColor -> Map BagColor [(BagColor, Int)] -> Int
countContents c mp = case Map.lookup c mp of 
                      Nothing -> error "Unknown bag color."
                      Just contents -> foldl (+) 1 (map (\(b, n) -> n * countContents b mp) contents)