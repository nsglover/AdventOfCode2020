module Puzzles.Day22 (Day) where

import Common.Solution
import qualified Common.Parsing as P

import Data.Sequence (Seq, Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence as S

import qualified Data.Set as Set

data Day = Day
instance Solve Day where
  part1 _ ls = show $ score $ combat $ parse ls
  part2 _ ls = show $ score $ recursive $ parse ls

parse :: [String] -> (Seq Int, Seq Int)
parse ls = let [p1, p2] = map (S.fromList . map read . drop 1) $ P.splitOn "" ls in (p1, p2)

score :: Seq Int -> Int
score = S.foldlWithIndex (\acc i s -> acc + (i + 1) * s) 0 . S.reverse

combat :: (Seq Int, Seq Int) -> Seq Int
combat (p1, Empty) = p1
combat (Empty, p2) = p2
combat (c1 :<| p1, c2 :<| p2) = if c1 > c2 then combat (p1 :|> c1 :|> c2, p2) else combat (p1, p2 :|> c2 :|> c1)

recursive :: (Seq Int, Seq Int) -> Seq Int
recursive p = case helper Set.empty p of Left s -> s; Right s -> s
  where helper _ (p1, Empty) = Left p1
        helper _ (Empty, p2) = Right p2
        helper s (l1@(c1 :<| p1), l2@(c2 :<| p2)) = 
          let scores = (score l1, score l2); s' = Set.insert scores s
          in if scores `Set.member` s then Left l1
             else if c1 <= S.length p1 && c2 <= S.length p2
                  then case helper s' (S.take c1 p1, S.take c2 p2) of
                         Left _ -> helper s' (p1 :|> c1 :|> c2, p2)
                         Right _ -> helper s' (p1, p2 :|> c2 :|> c1)
                  else if c1 > c2 then helper s' (p1 :|> c1 :|> c2, p2) else helper s' (p1, p2 :|> c2 :|> c1)