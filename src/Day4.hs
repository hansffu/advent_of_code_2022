module Day4 (solve) where

import Data.List.Split (splitOn)

solve :: IO ()
solve = do
  putStrLn "Day 4:"
  putStr "Part 1"
  part1
  putStr "Part 2"
  part2
  putStrLn "------------------------"

part1 :: IO ()
part1 = readFile filename >>= putStrLn . processInput1 . lines
  where
    -- filename = "input/day4.sample.txt"
    filename = "input/day4.input.txt"

part2 :: IO ()
part2 = readFile filename >>= putStrLn . processInput2 . lines
  where
    -- filename = "input/day4.sample.txt"
    filename = "input/day4.input.txt"

processInput1 :: [String] -> String
processInput1 input = show $ length $ filter id $ map (isFullyOverlapping . parseLine) input

isFullyOverlapping :: ((Int, Int), (Int, Int)) -> Bool
isFullyOverlapping ((start1, end1), (start2, end2)) = not (any (`notElem` r2) r1) || not (any (`notElem` r1) r2)
  where
    r1 = [start1 .. end1]
    r2 = [start2 .. end2]

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine input = (head ranges, ranges !! 1)
  where
    ranges = map parseRange (splitOn [','] input)
    parseRange range = (readInt a, readInt b)
      where
        a = takeWhile (/= '-') range
        b = drop (length a + 1) range

processInput2 :: [String] -> String
processInput2 input = show $ length $ filter id $ map (isAnyOverlap . parseLine) input

readInt :: String -> Int
readInt = read

isAnyOverlap :: ((Int, Int), (Int, Int)) -> Bool
isAnyOverlap ((start1, end1), (start2, end2)) = any (`elem` r2) r1
  where
    r1 = [start1 .. end1]
    r2 = [start2 .. end2]
