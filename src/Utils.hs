module Utils (commonSolve, commonSolveIO, InputType (..), debug, readInt, getInput) where

import Debug.Trace (traceShow)

getInput :: String -> IO [String]
getInput filename = lines <$> readFile filename

commonSolveIO ::  Int -> InputType -> ([String] -> IO ()) -> ([String] -> IO ()) -> IO ()
commonSolveIO day inputType part1 part2 = do
  input <- getInput filename
  putStrLn $ "Day " <> show day <> ":"
  putStrLn $ "Part 1:  "
  part1 input
  putStrLn $ "Part 2:  "
  part2 input
  putStrLn "------------------------"
  where
    filename = "input/day" <> show day <> "." <> show inputType <> ".txt"

commonSolve :: (Show a, Show b) =>  Int -> InputType -> ([String] -> a) -> ([String] -> b) -> IO ()
commonSolve day inputType part1 part2 = do
  input <- getInput filename
  putStrLn $ "Day " <> show day <> ":"
  putStrLn $ "Part 1:  " <> show (part1 input)
  putStrLn $ "Part 2:  " <> show (part2 input)
  putStrLn "------------------------"
  where
    filename = "input/day" <> show day <> "." <> show inputType <> ".txt"

-- where filename = "input/day6.input.txt"

data InputType = Input | Sample

instance Show InputType where
  show Input = "input"
  show Sample = "sample"

debug :: Show b => b -> b
debug a = traceShow a a

readInt :: String -> Int
readInt = read


