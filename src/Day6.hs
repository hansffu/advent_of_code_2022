module Day6 (solve) where

import Data.List (nub)
import Debug.Trace (traceShow)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 6 Input part1 part2

part1 :: [String] -> String
part1 input = show $ p1 $ head input

p1 :: String -> Int
p1 (a : b : c : d : xs) = if [a, b, c, d] == nub [a, b, c, d] then 4 else 1 + p1 (b : c : d : xs)
p1 xs = traceShow xs 0

part2 :: [String] -> String
part2 input = show $ p2 $ head input

p2 :: String -> Int
p2 input = if toCheck == nub toCheck then 14 else 1 + p2 (drop 1 input)
  where
    toCheck = take 14 input
