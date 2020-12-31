module Puzzles.Day5 (Day) where

import Common.Solution
import Data.List

data Day = Day
instance Solve Day where
  part1 _ ls = show $ maximum $ map (seatId . position) ls
  part2 _ ls = show $ findSeat $ sort $ map (seatId . position) ls

seatId (r, c) = 8 * r + c

position :: String -> (Int, Int)
position = position' (0, 127) (0, 7)
  where position' (loR, hiR) (loC, hiC) [] = (hiR, hiC)
        position' (loR, hiR) c ('F':xs) = position' (loR, loR + (hiR - loR) `div` 2) c xs
        position' (loR, hiR) c ('B':xs) = position' (loR + (hiR - loR) `div` 2, hiR) c xs
        position' r (loC, hiC) ('L':xs) = position' r (loC, loC + (hiC - loC) `div` 2) xs
        position' r (loC, hiC) ('R':xs) = position' r (loC + (hiC - loC) `div` 2, hiC) xs

findSeat :: [Int] -> Int
findSeat [] = error "No empty seats"
findSeat [x] = x
findSeat (x:y:xs) = if x + 1 == y then findSeat (y:xs) else x + 1