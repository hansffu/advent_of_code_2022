module Day5.Part1 (part1) where

import Data.Char (digitToInt, isSpace)
import Data.List (transpose)
import Data.List.Extra (trim)
import Data.List.Split (splitOn)

part1 :: IO ()
part1 = readFile filename >>= putStrLn . processInput1 . lines
  where
    filename = "src/Day5/input.txt"

processInput1 :: [String] -> String
-- processInput1 input = show initial
-- processInput1 input = show $ map head $ foldr moveCrates initial commands
processInput1 input = processCommands initial commands
  where
    parts = splitOn [""] input
    initialRaw = transpose $ head parts
    initial = map trim $ filter (\cs -> '[' `notElem` cs && ']' `notElem` cs && not (all isSpace cs)) initialRaw
    commands = map parseCommand $ parts !! 1

processCommands :: [String] -> [(Int, Int, Int)] -> String
processCommands state [] = map head state
processCommands state (c : cs) = processCommands newState cs
  where
    newState = moveCrates c state

parseCommand :: String -> (Int, Int, Int)
parseCommand input = (read $ parts !! 1, read $ parts !! 3, read $ parts !! 5)
  where
    parts = words input

moveCrates :: (Int, Int, Int) -> [String] -> [String]
moveCrates (number, from, to) state = do
  stack <- state
  let stackId = digitToInt $ last stack
  return $ newStack stackId stack
  where
    cratesToMove = take number $ state !! (from - 1)
    newStack :: Int -> String -> String
    newStack stackId stack
      | stackId == from = drop number stack
      | stackId == to = reverse cratesToMove ++ stack
      | otherwise = stack
