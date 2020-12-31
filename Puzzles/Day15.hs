module Puzzles.Day15 (Day) where

import Common.Solution
import qualified Common.Parsing as P
import Data.Map (Map)
import qualified Data.Map.Strict as M

data Day = Day
instance Solve Day where
  part1 _ ls = show $ run 2020 $ initialize $ map read $ P.splitOn ',' (head ls)
  part2 _ ls = show $ run 30000000 $ initialize $ map read $ P.splitOn ',' (head ls)

initialize :: [Int] -> (Int, Int, Map Int Int)
initialize = foldl (\(i, y, m) x -> (i + 1, x, if y == -1 then m else M.insert y i m)) (1, -1, M.empty)

run :: Int -> (Int, Int, Map Int Int) -> Int
run end init = result 
  where (_, result, _) = step init
        step s@(turn, lastSpoken, mp) = if turn > end then s else 
          case M.insertLookupWithKey (\k n _ -> n) lastSpoken turn mp of 
            (Just oldTurn, m') -> step (turn + 1, turn - oldTurn, m')
            (Nothing, m') -> step (turn + 1, 0, m')