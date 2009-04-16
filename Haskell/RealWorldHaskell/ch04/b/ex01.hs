import Data.Char (digitToInt)

asInt_fold :: String -> Integer
asInt_fold str = case str of
  ('-':xs) -> negate (asInt_fold xs)
  _        -> foldl fn 0 str
              where fn acc ch = acc * 10 + toInteger (digitToInt ch)