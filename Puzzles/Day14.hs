module Puzzles.Day14 (Day) where

import Common.Solution
import qualified Common.Parsing as P
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bits

data Day = Day
instance Solve Day where
  part1 _ ls = let (mem, _) = run1 (map parse ls) in show $ M.foldl (+) 0 $ M.map binStrToInt mem
  part2 _ ls = let (mem, _) = run2 (map parse ls) in show $ M.foldl (+) 0 $ M.map binStrToInt mem

data Op = MaskSet | MemSet String deriving Show

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = toBin (n `quot` 2) ++ [n `rem` 2]

toBinStr :: String -> String
toBinStr ns = let bs = foldl1 (++) $ map show $ toBin (read ns) in replicate (36 - length bs) '0' ++ bs

binStrToInt :: String -> Integer
binStrToInt s = sum $ zipWith (\n c -> n * (if c == '0' then 0 else 1)) (map (2^) [0..]) (reverse s)

parse :: String -> (Op, String)
parse s = let [x, y] = map P.trim (P.splitOn '=' s)
          in case take 4 x of "mask" -> (MaskSet, y)
                              _ -> (MemSet (P.splitOn '[' (P.removeAll ']' x) !! 1), toBinStr y)

combos :: String -> [String]
combos = helper []
  where helper l [] = [reverse l]
        helper l (x:xs) = if x == 'X' then helper ('0':l) xs ++ helper ('1':l) xs else helper (x:l) xs

applyMask :: Char -> String -> String -> String
applyMask c = zipWith (\m b -> if m == c then b else m)

run1 :: [(Op, String)] -> (Map String String, String)
run1 = foldl step (M.empty, "")
  where step (mem, _) (MaskSet, newMask) = (mem, newMask)
        step (mem, mask) (MemSet address, value) = (M.insert address (applyMask 'X' mask value) mem, mask)

setMem :: String -> String -> Map String String -> Map String String
setMem addr val m = foldl (\m' a -> M.insert a val m') m (combos addr)

run2 :: [(Op, String)] -> (Map String String, String)
run2 = foldl step (M.empty, "")
  where step (mem, _) (MaskSet, newMask) = (mem, newMask)
        step (mem, mask) (MemSet address, value) = (setMem (applyMask '0' mask (toBinStr address)) value mem, mask)