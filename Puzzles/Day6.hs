module Puzzles.Day6 (Day) where

import Common.Solution

data Day = Day
instance Solve Day where
  part1 _ ls = show $ foldl (+) 0 (map (counts unionIndicator) (parse ls))
  part2 _ ls = show $ foldl (+) 0 (map (counts intersectIndicator) (parse ls))

parse :: [String] -> [[String]]
parse = parse' []
  where parse' l [] = l
        parse' [] (x:xs) = parse' [[x]] xs
        parse' l@(y:ys) (x:xs) = case x of [] -> parse' ([]:l) xs; _ -> parse' ((x:y):ys) xs

unionIndicator :: [String] -> (Char -> Int)
unionIndicator [] = (\_ -> 0)
unionIndicator (x:xs) = (\c -> if c `elem` x then 1 else (unionIndicator xs) c)

intersectIndicator :: [String] -> (Char -> Int)
intersectIndicator [] = (\_ -> 1)
intersectIndicator (x:xs) = (\c -> if c `elem` x then (intersectIndicator xs) c else 0)

counts :: ([String] -> (Char -> Int)) -> [String] -> Int
counts indicator l = foldl (+) 0 (map (indicator l) ['a'..'z'])