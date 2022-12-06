module Day3 (solve) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 3 Input part1 part2

part1 :: [String] -> Int
part1 input = sum $ map (score . head . duplicates . groupBags) input

part2 :: [String] -> Int
part2 input = sum $ map (score . head . duplicates3) groups
 where
  groups = groupElves input

groupBags :: String -> (String, String)
groupBags s = splitAt groupLength s
 where
  groupLength = length s `div` 2

groupElves :: [String] -> [(String, String, String)]
groupElves (x : y : z : xs) = (x, y, z) : groupElves xs
groupElves _ = []

score :: Char -> Int
score c = fromMaybe 0 (c `elemIndex` (['a' .. 'z'] ++ ['A' .. 'Z'])) + 1

duplicates :: (String, String) -> [Char]
duplicates p = [x | x <- fst p, y <- snd p, x == y]

duplicates3 :: (String, String, String) -> [Char]
duplicates3 (a, b, c) = [x | x <- a, y <- b, z <- c, x == y && x == z]
