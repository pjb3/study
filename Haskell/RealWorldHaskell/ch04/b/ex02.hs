-- INCOMPLETE, NOT WORKING
import Data.Char (digitToInt)

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either str = case str of
  ('-':xs) -> negate (asInt_either xs)
  _        -> foldl fn (Right 0) str
              where fn acc ch
                | isDigit ch = Right ((Right acc) * 10 + (Right (toInteger (digitToInt ch))))
                | otherwise = Left ("non-digit '" ++ ch ++ "'")