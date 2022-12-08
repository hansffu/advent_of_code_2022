module Day8 (solve) where

import Data.Char (digitToInt)
import Data.List (transpose)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 8 Input part1 part2

part1 :: [String] -> Int
part1 input = length $ filter (isVisible matrix) trees
 where
  trees = [(x, y) | x <- [0 .. length (head input) - 1], y <- [0 .. length input - 1]]
  matrix = map (map digitToInt) input

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible matrix (x, y) = isVisibleInRow row x || isVisibleInRow column y
 where
  transposed = transpose matrix
  row = matrix !! y
  column = transposed !! x

isVisibleInRow :: [Int] -> Int -> Bool
isVisibleInRow xs x = (x == 0) || (x == (length xs - 1)) || (maximum before < e) || (maximum after < e)
 where
  e = xs !! x
  before = take x xs
  after = drop (x + 1) xs

part2 input = "TODO"

