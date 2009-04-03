makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome (x:[]) = [x, x]
makePalindrome (x:xs) = [x] ++ makePalindrome xs ++ [x] 