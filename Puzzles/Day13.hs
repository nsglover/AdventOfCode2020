module Puzzles.Day13 (Day) where

import Common.Solution
import qualified Common.Parsing as P
import Data.List
import Data.Ord

data Day = Day
instance Solve Day where
  part1 _ ls = show (m * w)
    where ((_, m), w) = minimumBy (comparing snd) $ zip (parse ls) $ map (mod (-read (ls !! 0)) . snd) (parse ls)
  part2 _ ls = show ((-n) `mod` k) where (n, k) = foldl1 reduce (parse ls)

parse :: [String] -> [(Integer, Integer)]
parse ls = map (\(i, s) -> (i, read s)) $ filter ((/= "x") . snd) $ zip [0..] $ P.splitOn ',' (ls !! 1)

bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b = helper (a, b) (1, 0) (0, 1)
  where helper (_, 0) (s, _) (t, _) = (s, t)
        helper (r1, r2) (s1, s2) (t1, t2) = helper (r2, new) (s2, s1 - q * s2) (t2, t1 - q * t2)
          where (new, q) = (r1 `mod` r2, r1 `div` r2)

reduce :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
reduce (i1, m1) (i2, m2) = ((i1 * m2 * b2 + i2 * m1 * b1) `mod` m', m') where (b1, b2) = bezout m1 m2; m' = m1 * m2