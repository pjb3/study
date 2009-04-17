takeWhileRecur :: (a -> Bool) -> [a] -> [a]
takeWhileRecur f [] = []
takeWhileRecur f (x:xs) 
  | f x = x : takeWhileRecur f xs
  | otherwise = []


takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr f xs = 
  foldr step [] xs
    where step n acc 
            | f n = n : acc
            | otherwise = []
