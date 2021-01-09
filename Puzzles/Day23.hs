module Puzzles.Day23 (Day) where

import Common.Solution
import Data.Foldable (toList)

import Data.Sequence (Seq, Seq (Empty, (:<|), (:|>)), (><))
import qualified Data.Sequence as S

data Day = Day
instance Solve Day where
  part1 _ ls = foldl1 (++) $ map show $ toList $ normalize $ simulate 100 $ parse ls
  --part 2 is way too slow but im too lazy to do mutable stuff
  part2 _ ls = show $ product $ S.drop 2 $ normalize $ simulate 10000000 $ extend $ parse ls

parse :: [String] -> Seq Int
parse = S.fromList . map (read . (:[])) . head

insertMany :: Int -> Seq Int -> Seq Int -> Seq Int
insertMany _ Empty s = s
insertMany i (x :<| xs) s = insertMany (i + 1) xs (S.insertAt i x s)

simulate :: Int -> Seq Int -> Seq Int
simulate 0 s = s
simulate n (x :<| xs) = 
  let (hold, rest) = (S.take 3 xs, S.drop 3 xs)
      dest 0 = dest 9
      dest k = case S.elemIndexL k rest of Nothing -> dest (k - 1); Just i -> i
  in simulate (n - 1) (insertMany (dest (x - 1) + 1) hold rest :|> x)

normalize :: Seq Int -> Seq Int
normalize (x :<| xs) = if x == 1 then xs else normalize (xs :|> x)

extend :: Seq Int -> Seq Int
extend s = let m = maximum s + 1 in s >< S.fromList [m..1000000]