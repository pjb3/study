splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = case xs of
  [] -> []
  _  -> let (h, t) = break f (dropWhile f xs)
        in [h] ++ splitWith f (dropWhile f t)