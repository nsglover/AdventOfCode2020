module Puzzles.Day2 (Day) where

import Common.Solution
import Common.Parsing as Parse

data Day = Day
instance Solve Day where
  part1 _ ls = show $ solve isValid1 ls
  part2 _ ls = show $ solve isValid2 ls

data Password = Password (Int, Int) Char String

parse :: String -> Password
parse s = 
  let
    s' = Parse.replaceChars ['-',':'] ' ' s
    ls = Parse.splitOn ' ' $ Parse.trim s'
  in Password (read (ls !! 0), read (ls !! 1)) ((ls !! 2) !! 0) (ls !! 3)

isValid1 :: Password -> Bool
isValid1 (Password (min, max) c pwd) =
  let
    count :: String -> Int
    count [] = 0
    count (x:xs) = if x == c then 1 + count xs else count xs
    
    num = count pwd
  in min <= num && num <= max

isValid2 :: Password -> Bool
isValid2 (Password (pos1, pos2) c pwd) = (pwd !! (pos1 - 1) == c) /= (pwd !! (pos2 - 1) == c)

solve :: (Password -> Bool) -> [String] -> Int
solve isValid input = foldl (\acc x -> if isValid x then acc + 1 else acc) 0 (map parse input)