module Day1 (solve) where

import Data.List.Split (splitOn)
import Data.List (sort)

solve :: IO ()
solve = do
  putStrLn "Day 1:"
  putStr "Part 1"
  part1
  putStr "Part 2"
  part2
  putStrLn "------------------------"


part1 :: IO ()
part1 = readFile filename >>= putStrLn . processInput1 . lines
  where
    -- filename = "input/day1.sample.txt"
    filename = "input/day1.input.txt"

processInput1 :: [String] -> String
processInput1 input = show $ maximum groups
  where
    groups = sumGroups input

part2 :: IO ()
part2 = readFile filename >>= putStrLn . processInput2 . lines
  where
    -- filename = "input/day1.sample.txt"
    filename = "input/day1.input.txt"

processInput2 :: [String] -> String
processInput2 input = show $ sum top3
  where
    groups = sumGroups input
    top3 = take 3 $ reverse $ sort groups


sumGroups :: [String] -> [Int]
sumGroups input = do
  group <- splitOn [""] input
  return $ sum $ map readInt group

readInt :: String -> Int
readInt = read
