module Common.Parsing(splitOn, trimLeft, trimRight, trimEdges, trim,
removeFirstN, removeAll, removeChars, replaceFirstN, replaceAll, replaceChars) where

-- | Splits a string into substrings separated by a given delimiter (the delimiter is removed)
splitOn :: Eq t => t -> [t] -> [[t]]
splitOn _ [] = []
splitOn c s = if null (fst split) then splitOn c rest else fst split : splitOn c (snd split)
  where split = break (c==) s
        (_:rest) = snd split

-- | Removes all trailing whitespace on the left of the string
trimLeft :: String -> String
trimLeft "" = ""
trimLeft s@(x:xs) = if x == ' ' then trimLeft xs else s

-- | Removes all trailing whitespace on the right of the string
trimRight :: String -> String
trimRight = reverse . trimLeft . reverse

-- | Removes all trailing whitespace on both ends of the string
trimEdges :: String -> String
trimEdges = trimRight . trimLeft

-- | Trims edges and removes almost all whitespace between words, but leaving one space between them
trim :: String -> String
trim = trim' . trimEdges
  where trim' :: String -> String
        trim' [] = []
        trim' [x] = [x]
        trim' (x:y:xs) = if x == ' ' then (if y == ' ' then trim' (y:xs) else x:(trim' (y:xs))) else x:(trim' (y:xs))

-- | Removes first n occurrences of the character c in string s
removeFirstN :: Char -> Int -> String -> String
removeFirstN _ _ "" = ""
removeFirstN _ 0 s = s
removeFirstN c n (x:xs) = if x == c then removeFirstN c (n - 1) xs else x:(removeFirstN c n xs)

-- | Removes all occurrences of the character c in string s
removeAll :: Char -> String -> String
removeAll _ [] = []
removeAll c (x:xs) = if x == c then removeAll c xs else x : removeAll c xs

-- | Removes all occurences of multiple given characters in string s
removeChars :: [Char] -> String -> String
removeChars [] s = s
removeChars (c:cs) s = removeChars cs (removeAll c s)

-- | Replaces first n occurrences of the character c with character c' in string s
replaceFirstN :: Char -> Char -> Int -> String -> String
replaceFirstN _ _ _ "" = ""
replaceFirstN _ _ 0 s = s
replaceFirstN c c' n (x:xs) = if x == c then c':(replaceFirstN c c' (n - 1) xs) else x:(replaceFirstN c c' n xs)

-- | Replaces all occurrences of the character c with character c' in string s
replaceAll :: Char -> Char -> String -> String
replaceAll _ _ [] = []
replaceAll c c' (x:xs) = if x == c then c':(replaceAll c c' xs) else x : replaceAll c c' xs

-- | Replaces all occurrences of multiple given characters with character c' in string s
replaceChars :: [Char] -> Char -> String -> String
replaceChars _ _ [] = []
replaceChars cs c' (x:xs) = if x `elem` cs then c':(replaceChars cs c' xs) else x : replaceChars cs c' xs