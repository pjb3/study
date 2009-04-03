intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse s (x:xs) = x ++ [s] ++ (intersperse s xs)