import System.Environment (getArgs)

myTranspose :: [[a]] -> [[a]]
myTranspose xs 
  | null xs = []
  | any null xs = []
  | otherwise = (map head xs) : myTranspose (map tail xs)

transposeLines :: String -> String
transposeLines s = unlines $ myTranspose (lines s)

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
        myFunction = transposeLines
