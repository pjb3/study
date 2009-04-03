myLength :: (Num b) => [a] -> b
myLength l = f l 0
    where f [] _ = 0 
          f (x:xs) i = 1 + (f xs i)
             