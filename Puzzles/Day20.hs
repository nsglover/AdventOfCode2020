module Puzzles.Day20 (Day) where

import Common.Solution
import qualified Common.Parsing as P
import Text.Printf ( printf )
import Data.Array ( Array, (!) )
import qualified Data.Array as A
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe ( catMaybes, fromJust, isNothing )

data Day = Day
instance Solve Day where
  part1 _ ls = show $ product $ map idnum $ findCorners $ parse ls
  part2 _ ls = show $ 
    let grid = assemble $ parse ls; n = size grid
    in roughness grid

data Tile = Tile { idnum :: Int, img :: Array Int (Array Int Char), access :: Int -> Int -> Char }
instance Show Tile where
  show (Tile id img f) = let idc = A.indices img
                         in printf "\nTile %d:\n" id ++ L.intercalate "\n" (map (\i -> [f i j | j <- idc]) idc)

type Grid = Array Int (Array Int Tile)
type View = (Int -> Int -> Char)

parse :: [String] -> [Tile]
parse = 
  let metadata (head:tail) = (read $ take 4 $ drop 5 head, tail)
      toArr xs = let rows = map (\r -> A.listArray (0, length r - 1) r) xs in A.listArray (0, length xs - 1) rows
  in map (\t -> let (id, dat) = metadata t; arr = toArr dat in Tile id arr (\i j -> (arr ! i) ! j)) . P.splitOn ""

rotateFn :: Int -> View -> View
rotateFn n f i j = f j (n - i)

rotate :: Tile -> Tile
rotate (Tile id img f) = let (_, n) = A.bounds img in Tile id img (rotateFn n f)

spin :: Int -> Tile -> Tile
spin n = foldr (.) id (replicate (n `mod` 4) rotate)

spinFn :: Int -> Int -> View -> View
spinFn n k = foldr (.) id (replicate (k `mod` 4) (rotateFn n))

reflectFn :: Int -> Int -> View -> View
reflectFn n k f = if even k then \i j -> f i (n - j) else \i j -> f (n - i) j

reflect :: Int -> Tile -> Tile
reflect k (Tile id img f) = let (_, n) = A.bounds img in Tile id img (reflectFn n k f)

edges :: Tile -> [(Int, String)]
edges (Tile _ img f) = let (_, n) = A.bounds img 
                       in zip [0..] $ map (`map` [0..n]) [f 0 . (n -), (`f` 0), f n, (`f` n) . (n -)]

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

neighbor :: Int -> Tile -> [Tile] -> Maybe Tile -- 0: top, 1: left, 2: bottom, 3: right
neighbor k t ts = 
  let l = zip ts (map edges ts); (_, e) = edges t !! k; e' = reverse e in helper e e' l
  where helper _ _ [] = Nothing
        helper e e' ((t', ys):xs) = if idnum t' == idnum t then helper e e' xs else
          case find ((e ==) . snd) ys of 
            Nothing -> case find ((e' ==) . snd) ys of 
                        Nothing -> helper e e' xs
                        Just (k', _) -> Just $ reflect k $ spin (k - k' + 2) (reflect k' t')
            Just (k', _) -> Just $ reflect k $ spin (k - k' + 2) t'

neighbors :: [Tile] -> Tile -> [Maybe Tile]
neighbors ts t = map (\i -> neighbor i t ts) [0..3]

findCorners :: [Tile] -> [Tile]
findCorners ts = map fst $ filter ((==2) . snd) $ zip ts $ map (length . catMaybes . neighbors ts) ts

topLeft :: [Tile] -> Tile
topLeft ts = let t = head (findCorners ts) in case take 2 (neighbors ts t) of [Nothing, Nothing] -> t
                                                                              [Just _, Nothing] -> spin (-1) t
                                                                              [Just _, Just _] -> spin (-2) t
                                                                              [Nothing, Just _] -> spin 1 t

assemble :: [Tile] -> Grid
assemble ts =
  let getChain dir tile = reverse (helper tile [tile])
        where helper s l = case neighbor dir s ts of Nothing -> l; Just s' -> helper s' (s':l)
      list = map (getChain 3) (getChain 2 (topLeft ts))
  in let rows = map (\r -> A.listArray (0, length r - 1) r) list in A.listArray (0, length list - 1) rows

view :: Grid -> Int -> Int -> Char
view grid i j = let (_, n') = A.bounds $ img $ (grid ! 0) ! 0; n = n' - 1
                    (qi, ri) = (quot i n, rem i n); (qj, rj) = (quot j n, rem j n)
                    (Tile _ _ f) = (grid ! qi) ! qj
                in f (1 + ri) (1 + rj)

size :: Grid -> Int
size grid = let (_, n) = A.bounds grid; (_, m) = A.bounds $ img $ (grid ! 0) ! 0 in (n + 1) * (m - 1) - 1

seaMonster :: (View, Int, Int)
seaMonster = let raw = ["                  # ",
                        "#    ##    ##    ###",
                        " #  #  #  #  #  #   "]
                 arr = let rs = map (\r -> A.listArray (0, length r - 1) r) raw in A.listArray (0, length raw - 1) rs
             in (\i j -> (arr ! i) ! j, length (head raw) - 1, length raw - 1)

display :: Grid -> String
display arr = let f = view arr; idc = [0..(size arr)] in "\n" ++ L.intercalate "\n" (map (\i -> [f i j | j <- idc]) idc)

markMonsters :: (View -> View) -> Grid -> Map (Int, Int) (Char, Bool)
markMonsters f grid = 
  let n = size grid; v = f (view grid); (mv, w, h) = seaMonster; monster = [(i, j, mv i j) | i <- [0..h], j <- [0..w]]
      offsets = [(a, b) | a <- [0..(n - h)], b <- [0..(n - w)]]
      foldFn m (a, b) = if all (\(i,j,c) -> c == ' ' || c == v (i + a) (j + b)) monster
        then foldl (\m' (i,j,c) -> let s = v (i + a) (j + b) in M.insert (i + a, j + b) (s, c == s) m') m monster
        else foldl (\m' (i,j,c) -> M.insertWith (\n o -> o) (i + a, j + b) (v (i + a) (j + b), False) m') m monster
  in foldl foldFn M.empty offsets

roughness :: Grid -> Int
roughness = helper 0 id
  where countMons f p grid = M.size $ M.filter (\(c, b) -> c == '#' && p b) $ markMonsters f grid
        helper i f grid
          | i == 4 = helper 0 (reflectFn (size grid) 0) grid
          | countMons f id grid == 0 = helper (i + 1) (rotateFn (size grid) . f) grid
          | otherwise = countMons f not grid
