module Week3.Week3 (part1, part2) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

part1 :: IO ()
part1 = readFile filename >>= putStrLn . processInput1 . lines
  where
    -- filename = "src/Week3/sample.txt"
    filename = "src/Week3/input.txt"

processInput1 :: [String] -> String
processInput1 input = show $ sum $ map (score . head . duplicates . groupBags) input

part2 :: IO ()
part2 = readFile filename >>= putStrLn . processInput2 . lines
  where
    filename = "src/Week3/input.txt"

-- filename = "src/Week3/sample.txt"

processInput2 :: [String] -> String
processInput2 input = show $ sum $ map (score . head . duplicates3) groups
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
