module Puzzles.Day4 (Day) where

import Common.Solution
import Common.Parsing

data Day = Day
instance Solve Day where
  part1 _ ls = show $ length $ filter isPassport $ map parse $ compress ls
  part2 _ ls = show $ length $ filter isValid $ filter isPassport $ map parse $ compress ls

fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

compress :: [String] -> [String]
compress = compress' []
  where compress' l [] = l
        compress' [] (x:xs) = compress' [x] xs
        compress' l@(y:ys) (x:xs) = if x == "" then compress' (x:l) xs else compress' ((x ++ " " ++ y):ys) xs

parse :: String -> [(String, String)]
parse s = map ((\x -> (x !! 0, x !! 1)) . splitOn ':') $ splitOn ' ' $ trim s

isPassport :: [(String, String)] -> Bool
isPassport ps = (foldl1 (+) (map (\(s,_) -> if s `elem` fields then 1 else 0) ps)) == length fields

validField :: (String, String) -> Bool
validField ("byr", s) = let y = read s in 1920 <= y && y <= 2002
validField ("iyr", s) = let y = read s in 2010 <= y && y <= 2020
validField ("eyr", s) = let y = read s in 2020 <= y && y <= 2030
validField ("hgt", s) = 
  let n = length s in (n >= 2) && (let (num, unit) = (take (n - 2) s, drop (n - 2) s) 
                                  in case unit of "cm" -> let y = read num in 150 <= y && y <= 193
                                                  "in" -> let y = read num in 59 <= y && y <= 76
                                                  _ -> False)
validField ("hcl", s) = (take 1 s == "#") && foldl1 (&&) (map validChar (drop 1 s))
  where validChar c = c `elem` ['0'..'9'] || c `elem` ['a'..'f']
validField ("ecl", s) = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField ("pid", s) = length s == 9 && read s >= 0
validField ("cid", _) = True
validField _ = False

isValid :: [(String, String)] -> Bool
isValid = foldl1 (&&) . map validField