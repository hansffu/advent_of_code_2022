module Day10(solve) where

import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 10 Input part1 part2

part1 :: [String] -> Int
part1 = sumResults . calcCycles  . parseInput
 where
  signalStrength cycles n = n * cycles !! n
  sumResults cycles = sum $ map (signalStrength cycles) [20, 60, 100, 140, 180, 220]
part2 _ = "todo"

data Command = Noop | Addx Int
  deriving (Show)

calcCycles :: [Command] -> [Int]
calcCycles cs = 0:  helper [1] cs
 where
  helper :: [Int] -> [Command] -> [Int]
  helper prev [] = prev
  helper prev (Noop : cs) = helper (prev ++ [last prev]) cs
  helper prev ((Addx n) : cs) = helper (prev ++ [lastX, lastX + n]) cs
   where
    lastX = last prev

parseInput :: [String] -> [Command]
parseInput [] = []
parseInput ("noop" : xs) = Noop : parseInput xs
parseInput (line : xs) =
  let w = words line
      n = read (w !! 1) :: Int
   in Addx n : parseInput xs
