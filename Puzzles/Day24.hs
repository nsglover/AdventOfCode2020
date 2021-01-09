module Puzzles.Day24 (Day) where

import Common.Solution

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

data Day = Day
instance Solve Day where
  part1 _ ls = show $ sum $ buildMap ls
  part2 _ ls = show $ sum $ fmap fst $ simulate 100 $ neighborMap $ buildMap ls

type Vec = (Int, Int, Int)
instance (Num t1, Num t2, Num t3) => Num (t1, t2, t3) where
  (+) (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
  (*) (x1, y1, z1) (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)
  negate (x, y, z) = (negate x, negate y, negate z)
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = (signum x, signum y, signum z)
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n)

parseVec :: String -> Vec
parseVec "e" = (1, -1, 0)
parseVec "se" = (0, -1, 1)
parseVec "sw" = (-1, 0, 1)
parseVec "w" = (-1, 1, 0)
parseVec "nw" = (0, 1, -1)
parseVec "ne" = (1, 0, -1)

parse :: String -> Vec
parse = sum . map parseVec . helper
  where helper [] = []
        helper [x] = [[x]]
        helper (x:y:xs) = if x `elem` ['n', 's'] then [x,y] : helper xs else [x] : helper (y:xs)

buildMap :: [String] -> Map Vec Int
buildMap = foldl (\m s -> M.insertWith (\_ old -> 1 - old) (parse s) 1 m) M.empty

neighbors :: Vec -> [Vec]
neighbors v = map (\s -> v + parseVec s) ["e", "se", "sw", "w", "nw", "ne"]

neighborMap :: Map Vec Int -> Map Vec (Int, [Vec])
neighborMap = M.mapWithKey (\k i -> (i, neighbors k))

addNeighbors :: Map Vec (Int, [Vec]) -> Map Vec (Int, [Vec])
addNeighbors m = fst $ M.mapAccum accumFn m m
  where insertNeighbors mp ns = M.insertWith (\_ x -> x) ns (0, neighbors ns) mp
        accumFn = \acc s@(i, nbors) -> if i == 1 then (foldl insertNeighbors acc nbors, s) else (acc, s)

step :: Map Vec (Int, [Vec]) -> (Int, [Vec]) -> (Int, [Vec])
step m p@(i, ns) = 
  let 
    numBlack = sum $ map (\k -> case M.lookup k m of Nothing -> 0; Just (x, _) -> x) ns
  in case i of 0 -> if numBlack == 2 then (1, ns) else p
               1 -> if numBlack == 0 || numBlack > 2 then (0, ns) else p

simulate :: Int -> Map Vec (Int, [Vec]) -> Map Vec (Int, [Vec])
simulate 0 m = m
simulate i m = let m' = addNeighbors m in simulate (i - 1) (M.map (step m') m')