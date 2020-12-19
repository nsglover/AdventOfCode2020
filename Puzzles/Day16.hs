module Puzzles.Day16 (Day) where

import Common.Solution
import qualified Common.Parsing as P

import Data.List (sort, sortBy, concat)
import Data.Ord (comparing)
import Data.Array (Array, (!))
import qualified Data.Array as A

data Day = Day
instance Solve Day where
  part1 _ ls = let 
                (fields, _, nearbyTickets) = parse ls
                u = union fields
                valid i = foldl (||) False $ map (within i) u
               in show $ sum $ concat $ map (filter (not . valid)) nearbyTickets
  part2 _ ls = let
                (fields, myTicket, nearbyTickets) = parse ls
                u = union fields
                valid i = foldl (||) False $ map (within i) u
                valids = filter (foldl (&&) True . map valid) nearbyTickets
                positions = determine $ sortBy (comparing (length . snd)) $ map (possible valids) fields
                relevant = map snd $ filter (\(s,_) -> let (y:_) = P.splitOn ' ' s in y == "departure") positions
               in show $ product $ map (myTicket !!) relevant

data Field = Field String (Int, Int) (Int, Int) deriving Show
type Ticket = [Int]

within :: Int -> (Int, Int) -> Bool
within i (a, b) = (a <= i && i <= b)

fitsField :: Field -> Int -> Bool
fitsField (Field _ r1 r2) i = (within i r1) || (within i r2)

parse :: [String] -> ([Field], Ticket, [Ticket])
parse ls =
  let
    (fields, rest) = break (""==) ls
    ([myTicket], nearbyTickets) = let (t1, t2) = break (""==) (drop 1 rest) in (drop 1 t1, drop 2 t2)

    parsePartialRange s = let [a, b] = P.splitOn '-' s in (read a, read b)
    parseField s = let [name, r] = P.splitOn ':' s; [r1, _, r2] = P.splitOn ' ' (P.trim r)
                   in Field name (parsePartialRange r1) (parsePartialRange r2)
    parseTicket = map read . P.splitOn ','
  in (map parseField fields, parseTicket myTicket, map parseTicket nearbyTickets)

toRaw :: [Field] -> [(Int, Int)]
toRaw = sort . foldl (\l (Field _ r1 r2) -> r1:r2:l) []

union :: [Field] -> [(Int, Int)]
union fs = foldl combine [] $ toRaw fs
  where combine [] y = [y]
        combine ((r1@(a,b)):xs) r2@(c,d)
          | (c <= b) = (min a c, max b d):xs
          | otherwise = r2:r1:xs

possible :: [Ticket] -> Field -> (String, [Int])
possible t f@(Field s _ _) =
  let
    tickets = A.listArray (0, length t - 1) (map (A.listArray (0, 19)) (map (zip [0..]) t))
    helper [] _ = error "Impossible!"
    helper idxs j = if (not . within j) (A.bounds tickets) then (s, idxs) else
      let
        curr = tickets ! j
        validIdxs = map fst $ filter (fitsField f . snd) (map (curr !) idxs)
      in helper validIdxs (j + 1)
  in helper [0..19] 0

determine :: [(String, [Int])] -> [(String, Int)]
determine = helper []
  where helper l [] = l
        helper l ((s,[p]):xs) = helper ((s,p):l) (map (\(s, ys) -> (s, filter (/= p) ys)) xs)