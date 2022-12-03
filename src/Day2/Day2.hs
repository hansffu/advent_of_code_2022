module Day2.Day2 where

part1 :: IO ()
part1 = readFile filename >>= putStrLn . processInput1 . lines
  where
    filename = "src/Day2/sample.txt"

-- filename = "src/Day2/input.txt"

part2 :: IO ()
part2 = readFile filename >>= putStrLn . processInput2 . lines
  where
    -- filename = "src/Day2/sample.txt"
    filename = "src/Day2/input.txt"

processInput1 :: [String] -> String
processInput1 input = show $ sum $ map calculateScore input

processInput2 :: [String] -> String
processInput2 input = show $ sum $ map (calculateScore . convertInput) input

calculateScore :: String -> Int
calculateScore ('A' : ' ' : 'X' : []) = 3 + 1
calculateScore ('A' : ' ' : 'Y' : []) = 6 + 2
calculateScore ('A' : ' ' : 'Z' : []) = 0 + 3
calculateScore ('B' : ' ' : 'X' : []) = 0 + 1
calculateScore ('B' : ' ' : 'Y' : []) = 3 + 2
calculateScore ('B' : ' ' : 'Z' : []) = 6 + 3
calculateScore ('C' : ' ' : 'X' : []) = 6 + 1
calculateScore ('C' : ' ' : 'Y' : []) = 0 + 2
calculateScore ('C' : ' ' : 'Z' : []) = 3 + 3

convertInput :: String -> String
convertInput ('A' : ' ' : 'X' : []) = "A Z"
convertInput ('A' : ' ' : 'Y' : []) = "A X"
convertInput ('A' : ' ' : 'Z' : []) = "A Y"
convertInput ('B' : ' ' : 'X' : []) = "B X"
convertInput ('B' : ' ' : 'Y' : []) = "B Y"
convertInput ('B' : ' ' : 'Z' : []) = "B Z"
convertInput ('C' : ' ' : 'X' : []) = "C Y"
convertInput ('C' : ' ' : 'Y' : []) = "C Z"
convertInput ('C' : ' ' : 'Z' : []) = "C X"
