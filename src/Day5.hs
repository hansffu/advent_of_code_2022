module Day5 (solve) where

import Data.Char (digitToInt, isSpace)
import Data.List (transpose)
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import Utils (InputType (..), commonSolve, readInt)

solve :: IO ()
solve = commonSolve 5 Input part1 part2

part1 :: [String] -> String
-- processInput1 input = show initial
-- processInput1 input = show $ map head $ foldr moveCrates initial commands
part1 input = processCommands1 initial commands
  where
    parts = splitOn [""] input
    initialRaw = transpose $ head parts
    initial = map trim $ filter (\cs -> '[' `notElem` cs && ']' `notElem` cs && not (all isSpace cs)) initialRaw
    commands = map parseCommand $ parts !! 1

processCommands1 :: [String] -> [(Int, Int, Int)] -> String
processCommands1 state [] = map head state
processCommands1 state (c : cs) = processCommands1 newState cs
  where
    newState = moveCrates1 c state

parseCommand :: String -> (Int, Int, Int)
parseCommand input = (read $ parts !! 1, read $ parts !! 3, read $ parts !! 5)
  where
    parts = words input

moveCrates1 :: (Int, Int, Int) -> [String] -> [String]
moveCrates1 (number, from, to) state = do
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


part2 :: [String] -> String
part2 input = processCommands2 initial commands
  where
    parts = splitOn [""] input
    initialRaw = transpose $ head parts
    initial = map trim $ filter (\cs -> '[' `notElem` cs && ']' `notElem` cs && not (all isSpace cs)) initialRaw
    commands = map parseCommand $ parts !! 1

processCommands2 :: [String] -> [(Int, Int, Int)] -> String
processCommands2 state [] = map head state
processCommands2 state (c : cs) = processCommands2 newState cs
  where
    newState = moveCrates2 c state

moveCrates2 :: (Int, Int, Int) -> [String] -> [String]
moveCrates2 (number, from, to) state = do
  stack <- state
  let stackId = digitToInt $ last stack
  return $ newStack stackId stack
  where
    cratesToMove = take number $ state !! (from - 1)
    newStack :: Int -> String -> String
    newStack stackId stack
      | stackId == from = drop number stack
      | stackId == to = cratesToMove ++ stack
      | otherwise = stack
