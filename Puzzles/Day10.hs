module Puzzles.Day10 (Day) where

import Common.Solution
import Data.List (sort, group)

data Day = Day
instance Solve Day where
  part1 _ ls = 
    let 
      folder (n1, n3) x = if x == 1 then (n1 + 1, n3) else (n1, n3 + 1)
      (ones, threes) = foldl folder (0, 0) (jumps (0 : (sort . map read) ls))
    in show (ones * (threes + 1))
  part2 _ ls =
    let
      j = jumps (0 : (sort . map read) ls)
    in show $ (product . map (waysToSum . length) . filter (\x -> head x == 1) . group) j

jumps :: [Int] -> [Int]
jumps [_] = []
jumps (x:y:xs) = y - x : jumps (y:xs)

waysToSum :: Int -> Int
waysToSum 1 = 1
waysToSum 2 = 2
waysToSum 3 = 4
waysToSum n = waysToSum (n - 1) + waysToSum (n - 2) + waysToSum (n - 3)

