module Utils (commonSolve, InputType (..), debug, readInt) where
import Debug.Trace (traceShow)

getInput :: String -> IO [String]
getInput filename = lines <$> readFile filename

commonSolve :: Int -> InputType -> ([String] -> String) -> ([String] -> String) -> IO ()
commonSolve day inputType part1 part2 = do
  input <- getInput filename
  putStrLn $ "Day " <> show day <> ":"
  putStrLn $ "Part 1:  " <> part1 input
  putStrLn $ "Part 2:  " <> part2 input
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
