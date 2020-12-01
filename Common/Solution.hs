module Common.Solution (Solve, part1, part2) where

class Solve t where
  -- | Prints the solution to parts 1 and 2 respectively of a given puzzle.
  -- | 2nd parameter: Puzzle input split into lines
  part1 :: t -> [String] -> String
  part2 :: t -> [String] -> String
