module Puzzles.Day1 (Day) where

import Common.Solution

data Day = Day
instance Solve Day where
  part1 _ ls = show $ sumToProduct 2 $ map read ls
  part2 _ ls = show $ sumToProduct 3 $ map read ls

findSublist :: Int -> Int -> [Int] -> Maybe [Int]
findSublist k n [] = if n == 0 && k == 0 then Just [] else Nothing
findSublist 1 n (x:xs) = if n == x then Just [x] else findSublist 1 n xs
findSublist k n (x:xs) = case findSublist (k - 1) (n - x) xs of Nothing -> findSublist k n xs 
                                                                Just ys -> Just (x:ys)

sumToProduct :: Int -> [Int] -> Int
sumToProduct k l = let (Just sublist) = findSublist k 2020 l in foldl1 (*) sublist
