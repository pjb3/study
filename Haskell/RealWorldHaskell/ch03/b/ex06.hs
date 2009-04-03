import Data.List

sortByLength :: (Ord a) => [[a]] -> [[a]]
sortByLength l = sortBy compareLength l
  where compareLength a b = (length a) `compare` (length b)