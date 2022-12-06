module Day4 (solve) where

import Data.List.Split (splitOn)
import Utils (InputType (..), commonSolve, readInt)

solve :: IO ()
solve = commonSolve 4 Input part1 part2

part1 :: [String] -> String
part1 input = show $ length $ filter id $ map (isFullyOverlapping . parseLine) input

part2 :: [String] -> String
part2 input = show $ length $ filter id $ map (isAnyOverlap . parseLine) input

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine input = (head ranges, ranges !! 1)
 where
  ranges = map parseRange (splitOn [','] input)
  parseRange range = (readInt a, readInt b)
   where
    a = takeWhile (/= '-') range
    b = drop (length a + 1) range

isFullyOverlapping :: ((Int, Int), (Int, Int)) -> Bool
isFullyOverlapping ((start1, end1), (start2, end2)) = not (any (`notElem` r2) r1) || not (any (`notElem` r1) r2)
 where
  r1 = [start1 .. end1]
  r2 = [start2 .. end2]

isAnyOverlap :: ((Int, Int), (Int, Int)) -> Bool
isAnyOverlap ((start1, end1), (start2, end2)) = any (`elem` r2) r1
 where
  r1 = [start1 .. end1]
  r2 = [start2 .. end2]
