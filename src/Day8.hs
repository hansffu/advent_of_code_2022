module Day8 (solve) where

import Data.Char (digitToInt)
import Data.List (transpose)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 8 Input part1 part2

type Tree = (Int, Int)

part1 :: [String] -> Int
part1 input = length $ filter (isVisible matrix) trees
 where
  trees = [(x, y) | x <- [0 .. length (head input) - 1], y <- [0 .. length input - 1]]
  matrix = map (map digitToInt) input

isVisible :: [[Int]] -> Tree -> Bool
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

part2 :: [String] -> Int
part2 input = maximum $ map (treeVisibility matrix) trees
 where
  trees = [(x, y) | x <- [0 .. length (head input) - 1], y <- [0 .. length input - 1]]
  matrix = map (map digitToInt) input

treeVisibility :: [[Int]] -> Tree -> Int
treeVisibility forest (x, y) = product $ map viewedTrees [left, right, up, down]
 where
  transposed = transpose forest
  row = forest !! y
  height = row !! x
  column = transposed !! x
  left = reverse $ take x row
  right = drop (x + 1) row
  up = reverse $ take y column
  down = drop (y + 1) column
  viewedTrees line =
    let untilBlocked = takeWhile (< height) line
     in min (length line) (succ $ length untilBlocked)
