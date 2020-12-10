module Puzzles.Day9 (Day) where

import Common.Solution
import Data.Sequence (Seq, Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq

data Day = Day
instance Solve Day where
  part1 _ ls = show $ findError $ Seq.fromList $ map read ls
  part2 _ ls = show $ 
    let 
      s = Seq.fromList (map read ls) 
      Just s' = findContSubseq (findError s) s
      (min :<| (_ :|> max)) = Seq.sort s'
    in min + max

findSubseq :: Int -> Int -> Seq Int -> Maybe (Seq Int)
findSubseq k n Seq.Empty = if n == 0 && k == 0 then Just Seq.Empty else Nothing
findSubseq 1 n (x :<| xs) = if n == x then Just (Seq.singleton x) else findSubseq 1 n xs
findSubseq k n (x :<| xs) = case findSubseq (k - 1) (n - x) xs of Nothing -> findSubseq k n xs 
                                                                  Just ys -> Just (x :<| ys)

findContSubseq :: Int -> Seq Int -> Maybe (Seq Int)
findContSubseq k Seq.Empty = Nothing
findContSubseq k s@(x :<| xs) = case helper k s of Nothing -> findContSubseq k xs
                                                   m@(Just s') -> if Seq.length s' >= 2 then m else findContSubseq k xs
  where helper n (x :<| xs) 
          | x > n = Nothing
          | x == n = Just (Seq.singleton x)
          | otherwise = case helper (n - x) xs of Nothing -> Nothing
                                                  Just s -> Just (x :<| s)
                      

findError :: Seq Int -> Int
findError dat = findError' (Seq.take 25 dat) (Seq.drop 25 dat)
  where findError' prev@(_ :<| ps) (x :<| xs) = case findSubseq 2 x prev of Nothing -> x
                                                                            Just _ -> findError' (ps :|> x) xs