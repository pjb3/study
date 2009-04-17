-- takeWhileRecur :: (a -> Bool) -> [a] -> [a]
-- takeWhileRecur f [] = []
-- takeWhileRecur f (x:xs) 
--   | f x = x : takeWhileRecur f xs
--   | otherwise = []
-- 

--takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr f xs = let step n acc 
                            | f n = n : acc
                            | otherwise = []
                      in foldr step [] xs


-- Ugh...Why doesn't this compile!
-- takeWhileFoldr f xs = foldr step [] xs
--                         where step n acc 
--                                 | f n = n : acc
--                                 | otherwise = []
