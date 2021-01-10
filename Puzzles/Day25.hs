module Puzzles.Day25 (Day) where

import Common.Solution

data Day = Day
instance Solve Day where
  part1 _ ls = show $ transform cardKey (modularLog 7 doorKey) where (cardKey, doorKey) = parse ls
  part2 _ ls = "Free!"

parse :: [String] -> (Int, Int)
parse [x, y] = (read x, read y)

modularLog :: Int -> Int -> Int
modularLog base target = helper 1 0
  where helper n i = if n == target then i else helper (n * base `mod` 20201227) (i + 1)

transform :: Int -> Int -> Int
transform s n = helper 1 n
  where helper v 0 = v
        helper v n = helper (v * s `mod` 20201227) (n - 1)