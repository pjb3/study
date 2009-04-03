--Seems to me this should work, but it don't
--mean :: (Num a, Fractional b) => [a] -> b
mean :: [Double] -> Double
mean l = sum l / len l
  where sum [] = 0
        sum (x:xs) = x + sum xs
        len [] = 0
        len (x:xs) = 1 + len xs