myConcat :: [[a]] -> [a]
myConcat l = foldr (++) [] l