module Utils (commonSolve) where

getInput :: String -> IO [String]
getInput filename = lines <$> readFile filename

commonSolve :: Int -> ([String] -> String) -> ([String] -> String) -> IO ()
commonSolve day part1 part2 = do
  input <- getInput filename
  putStrLn $ "Day " <> show day <> ":"
  putStr "Part 1:  "
  putStrLn $ part1 input
  putStr "Part 2:  "
  putStrLn $ part2 input
  putStrLn "------------------------"
  where
    filename = "input/day" <> show day <> ".sample.txt"

-- where filename = "input/day6.input.txt"
