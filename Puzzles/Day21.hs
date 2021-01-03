module Puzzles.Day21 (Day) where

import Common.Solution
import qualified Common.Parsing as P

import qualified Data.List as L

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Ord as O

data Day = Day
instance Solve Day where
  part1 _ ls = show $ let labels = map parse ls; sol = S.fromList $ map snd $ solve (definites labels)
                      in sum $ map (S.size . (`S.difference` sol) . snd) labels
  part2 _ ls = L.intercalate "," $ map snd $ L.sortBy (O.comparing fst) $ solve $ definites $ map parse ls

type Label = (Set String, Set String) -- (allergens, ingredients)

parse :: String -> Label
parse s = let [left, right] = P.splitOn "(contains" $ P.splitOn ' ' $ P.removeChars [',', ')'] s
          in (S.fromList right, S.fromList left)

definites :: [Label] -> [(String, Set String)]
definites ls = let allAllergens = S.toList $ S.unions $ map fst ls
                   containing a = map snd $ filter (\(as, _) -> a `S.member` as) ls
               in map (\a -> (a, foldl1 S.intersection $ containing a)) allAllergens

solve :: [(String, Set String)] -> [(String, String)]
solve [] = []
solve ds = let ((a,s):xs) = L.sortBy (\(_,s1) (_,s2) -> compare (S.size s1) (S.size s2)) ds
           in (a, head $ S.toList s) : solve (map (\(a', s') -> (a', s' `S.difference` s)) xs)