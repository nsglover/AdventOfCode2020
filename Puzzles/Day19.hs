module Puzzles.Day19 (Day) where

import Common.Solution
import qualified Common.Parsing as P
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S


data Day = Day
instance Solve Day where
  part1 _ ls = show $  
    let (rules, rest) = break (=="") ls; messages = drop 1 rest; getSet n = matchings (ruleMap rules) n
    in length $ filter (matchNormal (getSet 42) (getSet 31)) messages
  part2 _ ls = show $
    let (rules, rest) = break (=="") ls; messages = drop 1 rest; getSet n = matchings (ruleMap rules) n
    in length $ filter (matchRecursive (getSet 42) (getSet 31)) messages

type Rule = Either String [[Int]]

parse :: String -> (Int, Rule)
parse s = let [rule, matches] = P.splitOn ':' s in (read rule, 
            if '\"' `elem` matches then Left (P.removeAll '\"' $ P.trim matches)
            else Right (map (map (read . P.trim) . P.splitOn ' ') (P.splitOn '|' matches)))

ruleMap :: [String] -> Map Int Rule
ruleMap ls = M.fromList (map parse ls)

combine :: Set String -> Set String -> Set String
combine l r = S.map (uncurry (++)) (S.cartesianProduct l r)

matchings :: Map Int Rule -> Int -> Set String
matchings m r = case m ! r of Left s -> S.singleton s
                              Right l -> (S.unions . S.fromList) (map (foldl1 combine . map (matchings m)) l)

zipDrop :: String -> String -> Maybe String
zipDrop "" ys = Just ys
zipDrop _ "" = Nothing
zipDrop (x:xs) (y:ys) = if x == y then zipDrop xs ys else Nothing

matchPart :: String -> [String] -> Maybe String
matchPart s [] = Nothing
matchPart s (x:xs) = case zipDrop x s of Nothing -> matchPart s xs; p -> p

matchNormal :: Set String -> Set String -> String -> Bool
matchNormal ls rs s = case (do { s' <- matchPart s (S.toList ls);
                                 s'' <- matchPart s' (S.toList ls);
                                 matchPart s'' (S.toList rs) }) 
                      of Nothing -> False; Just s' -> s' == ""

matchRecursive :: Set String -> Set String -> String -> Bool
matchRecursive ls rs s = 
  let
    (s', n1) = helper 0 s (S.toList ls)
    (s'', n2) = helper 0 s' (S.toList rs)
  in s'' == "" && n2 < n1 && n1 > 0 && n2 > 0
  where helper n x set = case matchPart x set of Just z -> helper (n + 1) z set
                                                 Nothing -> (x, n)