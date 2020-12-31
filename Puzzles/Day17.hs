module Puzzles.Day17 (Day) where

import Common.Solution
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List (sort, groupBy, intercalate)

data Day = Day
instance Solve Day where
  part1 _ ls = show $ countActive 3 ls
  part2 _ ls = show $ countActive 4 ls

type Point = (Int, Int, Int, Int)
type State = (Char, [Point])

toString :: Int -> Map Point State -> String
toString z m =
  let
    plane = M.map fst $ M.filterWithKey (\(_, _, z', w') _ -> z == z' && z == w') m
    groups = groupBy (\(x1,_,_,_) (x2,_,_,_) -> x1 == x2) $ sort $ M.keys plane
  in "\n" ++ intercalate "\n" (map (map (plane !)) groups)

points :: [String] -> [Point]
points ls = let (rows, cols) = (length ls - 1, length (head ls) - 1) in [(i, j, 0, 0) | i <- [0..rows], j <- [0..cols]]

neighbors :: Int -> Point -> [Point]
neighbors d (x,y,z,w) = if d == 4 then [(x + i,y + j, z + k, w + l) | i <- [(-1)..1], j <- [(-1)..1], 
                                                                      k <- [(-1)..1], l <- [(-1)..1], 
                                                                      (i, j, k, l) /= (0, 0, 0, 0)]
  else [(x + i, y + j, z + k, 0) | i <- [(-1)..1], j <- [(-1)..1], k <- [(-1)..1], (i, j, k) /= (0, 0, 0)]

initialState :: Int -> [String] -> Map Point State
initialState d ls = M.fromList $ map (\p@(i,j,_,_) -> (p, ((ls !! i) !! j, neighbors d p))) $ points ls

step :: Map Point State -> State -> State
step m p@(c, ns) =
  let 
    neighbors = map (\k -> case M.lookup k m of Nothing -> '.'; Just (x, _) -> x) ns
    count = length (filter (=='#') neighbors)
  in case c of '.' -> if count == 3 then ('#', ns) else p
               '#' -> if count == 2 || count == 3 then p else ('.', ns)

addNeighbors :: Int -> Map Point State -> Map Point State
addNeighbors d m = let (m', _) = M.mapAccum accumFn m m in m'
  where insertNeighbors mp ns = (\m' k -> M.insertWith (\_ o -> o) k ('.', neighbors d k) m') mp ns
        accumFn = \acc s@(c, nbors) -> case c of '#' -> (foldl insertNeighbors acc nbors, s); _ -> (acc, s)

simulate :: Int -> Int -> Map Point State -> Map Point State
simulate d i m = if i == 0 then m else let m' = addNeighbors d m in simulate d (i - 1) (M.map (step m') m')

countActive :: Int -> [String] -> Int
countActive d ls = M.foldr (\(c, _) i -> if c == '#' then i + 1 else i) 0 $ simulate d 6 $ initialState d ls