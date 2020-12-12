module Puzzles.Day12 (Day) where

import Common.Solution

data Day = Day
instance Solve Day where
  part1 _ = show . finalDist (direction 'E') move1
  part2 _ = show . finalDist (10, 1) move2

parse :: [String] -> [(Char, Int)]
parse = map (\(x : xs) -> (x, read xs))

instance (Num t1, Num t2) => Num (t1, t2) where
  (+) (x, y) (z, w) = (x + z, y + w)
  (*) (x, y) (z, w) = (x * z, y * w)
  negate (x, y) = (negate x, negate y)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger n = (fromInteger n, fromInteger n)

type Vec = (Int, Int)
data Ship = Ship Vec Vec -- Position, Velocity for Part 1; Position, Waypoint for Part 2;

direction :: Char -> Vec
direction c = case c of 'N' -> (0, 1); 'E' -> (1, 0); 'S' -> (0, -1); 'W' -> (-1, 0)

rotate :: Int -> Vec -> Vec
rotate degrees (x, y) =
  let
    n = degrees `div` 90
    c = ((-1)^(n `div` 2)) * ((1 + (-1)^n) `div` 2)
    s = ((-1)^(n `div` 2)) * ((1 - (-1)^n) `div` 2)
  in (x * c - y * s, x * s + y * c)

move1 :: Ship -> (Char, Int) -> Ship
move1 (Ship p v) (c, n)
  | c `elem` "NESW" = Ship (p + (n, n) * (direction c)) v
  | otherwise = case c of 'L' -> Ship p (rotate n v)
                          'R' -> Ship p (rotate (360 - n) v)
                          'F' -> Ship (p + (n, n) * v) v

move2 :: Ship -> (Char, Int) -> Ship
move2 s@(Ship p v) o@(c, n)
  | c `elem` "NESW" = let Ship v' p' = move1 (Ship v p) o in Ship p' v'
  | otherwise = move1 s o

finalDist :: (Int, Int) -> (Ship -> (Char, Int) -> Ship) -> [String] -> Int
finalDist initial moveFn ls = let Ship (x, y) _ = foldl moveFn (Ship (0, 0) initial) (parse ls) in abs x + abs y