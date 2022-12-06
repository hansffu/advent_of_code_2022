module Day1 (solve) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 1 Input part1 part2

part1 :: [String] -> String
part1 input = show $ maximum groups
  where
    groups = sumGroups input

part2 :: [String] -> String
part2 input = show $ sum top3
  where
    groups = sumGroups input
    top3 = take 3 $ reverse $ sort groups

sumGroups :: [String] -> [Int]
sumGroups input = do
  group <- splitOn [""] input
  return $ sum $ map readInt group

readInt :: String -> Int
readInt = read
