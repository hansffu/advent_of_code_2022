module Day5 (solve) where

import Data.Char (digitToInt, isSpace)
import Data.List (transpose)
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import Utils (InputType (..), commonSolve)

type PickUpFunction = [Char] -> [Char]

solve :: IO ()
solve = commonSolve 5 Input part1 part2

part1 :: [String] -> String
part1 = solution reverse

part2 :: [String] -> String
part2 = solution id

solution :: PickUpFunction -> [String] -> String
solution pickUp = process
  where
    process :: [String] -> String
    process input = processCommands initial commands
      where
        parts = splitOn [""] input
        initial =
          [ trim x
            | x <- transpose $ head parts,
              '[' `notElem` x,
              ']' `notElem` x,
              not (all isSpace x)
          ]
        commands = map parseCommand $ parts !! 1

    processCommands :: [String] -> [(Int, Int, Int)] -> String
    processCommands state [] = map head state
    processCommands state (c : cs) = processCommands newState cs
      where
        newState = moveCrates c state

    moveCrates :: (Int, Int, Int) -> [String] -> [String]
    moveCrates (number, from, to) state = do
      stack <- state
      let stackId = digitToInt $ last stack
      return $ newStack stackId stack
      where
        cratesToMove = pickUp $ take number $ state !! (from - 1)
        newStack :: Int -> String -> String
        newStack stackId stack
          | stackId == from = drop number stack
          | stackId == to = cratesToMove ++ stack
          | otherwise = stack

parseCommand :: String -> (Int, Int, Int)
parseCommand input = (read $ parts !! 1, read $ parts !! 3, read $ parts !! 5)
  where
    parts = words input
