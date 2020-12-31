module Puzzles.Day8 (Day) where

import Common.Solution
import Common.Parsing
import Data.Array (Array, (!))
import qualified Data.Array as A

data Day = Day
instance Solve Day where
  part1 _ ls = let (False, acc) = runSafe (parse ls) in show acc
  part2 _ ls = show (runToTermination (parse ls))

parse :: [String] -> Array Int (String, Int)
parse ls = A.listArray (0, length ls - 1) $ map (toPair . splitOn ' ' . removeAll '+') ls
  where toPair xs = (head xs, read (xs !! 1))

runSafe :: Array Int (String, Int) -> (Bool, Int)
runSafe t = helper 0 0 (fmap (\(s, n) -> (s, n, 0)) t)
  where helper ix acc tape = if let (_, n) = A.bounds tape in ix >= n then (True, acc) else
          let
            (op, val, visits) = tape ! ix
            newTape = A.accum (\(s, n, v) k -> (s, n, v + k)) tape [(ix, 1)]
          in if visits == 1 then (False, acc) else case op of "acc" -> helper (ix + 1) (acc + val) newTape
                                                              "jmp" -> helper (ix + val) acc newTape
                                                              "nop" -> helper (ix + 1) acc newTape

runToTermination :: Array Int (String, Int) -> Int
runToTermination = helper 0
  where helper ix tape =
          let
            (op, _) = tape ! ix
            modified = if op == "acc" then tape else
              A.accum (\(s, n) _ -> (case s of "jmp" -> "nop"; "nop" -> "jmp", n)) tape [(ix, ())]
            (terminated, acc) = runSafe modified
          in if terminated then acc else helper (ix + 1) tape