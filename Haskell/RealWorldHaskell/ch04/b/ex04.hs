takeWhileRecur :: (a -> Bool) -> [a] -> [a]
takeWhileRecur f [] = []
takeWhileRecur f (x:xs) 
  | f x = x : takeWhileRecur f xs
  | otherwise = []

-- Ugh...Why doesn't this compile!
takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr f xs = foldr step [] xs
  where step n acc 
    | f n = n : acc
    | otherwise = []
