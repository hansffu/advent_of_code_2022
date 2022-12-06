module Day6 (solve) where

import Data.List (nub)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 6 Input part1 part2

part1 :: [String] -> Int
part1 = checkFirst 4 . head

part2 :: [String] -> Int
part2 = checkFirst 14 . head

checkFirst :: Int -> String -> Int
checkFirst numToCheck input = if toCheck == nub toCheck then numToCheck else 1 + checkFirst numToCheck (drop 1 input)
 where
  toCheck = take numToCheck input
