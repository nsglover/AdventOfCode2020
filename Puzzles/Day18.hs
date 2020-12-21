module Puzzles.Day18 (Day) where

import Common.Solution
import qualified Common.Parsing as P

data Day = Day
instance Solve Day where
  part1 _ ls = show $ sum $ map (eval '_') ls
  part2 _ ls = show $ sum $ map (eval '*') ls

breakOp :: String -> Either String (String, Char, String) 
breakOp = helper 0 [] . P.removeAll ' '
  where helper _ l [] = Left (reverse l)
        helper n l (x:xs) 
          | (n == 0) = case x of y | (y == '*' || y == '+') -> Right (reverse l, y, xs)
                                 '(' -> helper (n + 1) l xs
                                 _ -> helper n (x:l) xs
          | otherwise = case x of '(' -> helper (n + 1) (x:l) xs
                                  ')' -> helper (n - 1) (if n == 1 then l else x:l) xs
                                  _ -> helper n (x:l) xs

splitOps :: Char -> String -> ([String], [Char])
splitOps prec s = let (exps, ops) = helper ([], []) s in (reverse exps, reverse ops)
  where helper (exps, ops) s = case breakOp s of Left l -> (l:exps, ops)
                                                 Right (l, c, r) -> if c == prec then (r:l:exps, c:ops)
                                                                    else helper (l:exps, c:ops) r

charToOp :: Char -> (Integer -> Integer -> Integer)
charToOp c = case c of '*' -> (*); '+' -> (+)

eval :: Char -> String -> Integer
eval prec [c] = read [c]
eval prec s = foldl (\acc (n, op) -> acc `op` n) (head exps) (zip (tail exps) (map charToOp os))
  where (es, os) = (splitOps prec) s; exps = map (eval prec) es