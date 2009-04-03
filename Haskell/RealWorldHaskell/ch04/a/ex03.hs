import Data.Char
import System.Environment (getArgs)

firstWord :: String -> String
firstWord s = fst $ break isSpace s

firstWords :: String -> String
firstWords xs = unlines $ map firstWord (lines xs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = firstWords