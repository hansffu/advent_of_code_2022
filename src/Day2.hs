module Day2 (solve) where

import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 2 Input part1 part2

part1 :: [String] -> String
part1 input = show $ sum $ map calculateScore input

part2 :: [String] -> String
part2 input = show $ sum $ map (calculateScore . convertInput) input

calculateScore :: String -> Int
calculateScore ['A', ' ', 'X'] = 3 + 1
calculateScore ['A', ' ', 'Y'] = 6 + 2
calculateScore ['A', ' ', 'Z'] = 0 + 3
calculateScore ['B', ' ', 'X'] = 0 + 1
calculateScore ['B', ' ', 'Y'] = 3 + 2
calculateScore ['B', ' ', 'Z'] = 6 + 3
calculateScore ['C', ' ', 'X'] = 6 + 1
calculateScore ['C', ' ', 'Y'] = 0 + 2
calculateScore ['C', ' ', 'Z'] = 3 + 3
calculateScore _ = 0

convertInput :: String -> String
convertInput ['A', ' ', 'X'] = "A Z"
convertInput ['A', ' ', 'Y'] = "A X"
convertInput ['A', ' ', 'Z'] = "A Y"
convertInput ['B', ' ', 'X'] = "B X"
convertInput ['B', ' ', 'Y'] = "B Y"
convertInput ['B', ' ', 'Z'] = "B Z"
convertInput ['C', ' ', 'X'] = "C Y"
convertInput ['C', ' ', 'Y'] = "C Z"
convertInput ['C', ' ', 'Z'] = "C X"
convertInput _ = ""
