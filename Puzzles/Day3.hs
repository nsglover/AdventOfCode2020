module Puzzles.Day3 (Day) where

import Common.Solution
import Data.Array as A

data Day = Day
instance Solve Day where
  part1 _ ls = show $ countTrees (parse ls) (1, 3)
  part2 _ ls = show $
    let 
      mp = parse ls
      slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)] 
    in foldl1 (*) (map (countTrees mp) slopes)

data Terrain = Snow | Tree deriving (Show, Eq)
type Map = Array Int (Array Int Terrain)
type Position = (Int, Int)
type Slope = (Int, Int)

add :: Position -> Slope -> Position
add (x, y) (z, w) = (x + z, y + w)

parse :: [String] -> Map
parse ls = A.listArray (0, length ls - 1) (map parseOne ls)
  where parseOne s = A.listArray (0, length s - 1) (map (\c -> if c == '.' then Snow else Tree) s)

readMap :: Map -> Position -> Maybe Terrain
readMap mp (i, j) = if 0 <= i && i <= m then Just ((mp A.! i) A.! (j `mod` (n + 1))) else Nothing
  where (_, m) = A.bounds mp
        (_, n) = A.bounds (mp A.! 0)

countTrees :: Map -> Slope -> Int
countTrees = 
  let
    countTrees' pos n mp slope = 
      case readMap mp pos of Nothing -> n
                             Just Snow -> countTrees' (add pos slope) n mp slope
                             Just Tree -> countTrees' (add pos slope) (n + 1) mp slope
  in countTrees' (0, 0) 0
    