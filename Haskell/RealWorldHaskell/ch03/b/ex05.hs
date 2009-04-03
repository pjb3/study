palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome (x:[]) = True
palindrome (x:y:[]) = x == y
palindrome (x:y:z:[]) = x == z
palindrome (x:xs) = x == (last xs) && palindrome (init xs)