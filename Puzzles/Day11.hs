module Puzzles.Day11 (Day) where

import Common.Solution
import Data.Map (Map, (!))
import qualified Data.Map as M

data Day = Day
instance Solve Day where
  part1 _ ls = show $ countOccupied 4 adjacencyMap ls
  part2 _ ls = show $ countOccupied 5 visibilityMap ls

type Value = (Char, [(Int, Int)])

points :: [String] -> [(Int, Int)]
points ls = let (rows, cols) = (length ls - 1, length (head ls) - 1) in [(i, j) | i <- [0..rows], j <- [0..cols]]

inBounds :: (Int, Int) -> [String] -> Bool
inBounds (i, j) ls = 0 <= i && i < length ls && 0 <= j && j < length (head ls)

neighborlessMap :: [String] -> Map (Int, Int) Value
neighborlessMap ls = 
  M.fromList $ filter (\(_, (c, _)) -> c /= '.') $ map (\(i, j) -> ((i, j), ((ls !! i) !! j, []))) $ points ls

adjacencyMap :: [String] -> Map (Int, Int) Value
adjacencyMap ls = 
  let
    m = neighborlessMap ls
    keys = M.keys m
    mapFn (i, j) (c, _) = 
      let directions = [(i + x, j + y) | x <- [(-1)..1], y <- [(-1)..1], (x, y) /= (0, 0)]
      in (c, filter (\(z, w) -> (z, w) `elem` keys) directions)
  in M.mapWithKey mapFn m

visibilityMap :: [String] -> Map (Int, Int) Value
visibilityMap ls = 
  let
    m = neighborlessMap ls
    keys = M.keys m
    seek (i, j) inc@(dx, dy) =
      let next = (i + dx, j + dy)
      in if inBounds next ls then (if next `elem` keys then Just next else seek next inc) else Nothing
    mapFn (i, j) (c, _) =
      let directions = [(x, y) | x <- [(-1)..1], y <- [(-1)..1], (x, y) /= (0, 0)]
      in (c, foldl (\ns z -> case z of Just z' -> z':ns; _ -> ns) [] $ map (seek (i, j)) directions)
  in M.mapWithKey mapFn m

step :: Int -> Map (Int, Int) Value -> Bool -> (Int, Int) -> Value -> (Bool, Value)
step i m b idx v@(c, nidxs) =
  let neighbors = map (fst . (m !)) nidxs
  in case c of 'L' -> case filter (=='#') neighbors of [] -> (False, ('#', nidxs)); _ -> (b, v)
               '#' -> if length (filter (=='#') neighbors) >= i then (False, ('L', nidxs)) else (b, v)

simulate :: Int -> Map (Int, Int) Value -> Map (Int, Int) Value
simulate i m = let (b, m') = M.mapAccumWithKey (step i m) True m in if b then m' else simulate i m'

countOccupied :: Int -> ([String] -> Map (Int, Int) Value) -> [String] -> Int
countOccupied n f ls = M.foldr (\(c, _) i -> if c == '#' then i + 1 else i) 0 $ simulate n $ f ls