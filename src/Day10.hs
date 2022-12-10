module Day10 (solve) where

import Utils (InputType (..), commonSolveIO)
import Data.List.Split (chunksOf)
import Data.List.Utils (join)

solve :: IO ()
solve = commonSolveIO 10 Input part1 part2

part1 :: [String] -> IO ()
part1 = print . sumResults . calcCycles . parseInput
 where
  signalStrength cycles n = n * cycles !! (n - 1)
  sumResults cycles = sum $ map (signalStrength cycles) [20, 60, 100, 140, 180, 220]

part2 :: [String] -> IO ()
part2 input = putStrLn  $ join "\n" $ map drawLine $ chunksOf 40 sprites
 where
  cycles = (calcCycles . parseInput) input
  sprites = createSprites cycles

drawLine :: [[Int]] -> [Char]
drawLine bs = map (\i -> if i `elem` (bs !! i) then '#' else '.') [0..39]

createSprites :: [Int] -> [[Int]]
createSprites ([]) = []
createSprites (0:bs) = [0,1]: createSprites bs
createSprites (39:bs) = [38, 39]: createSprites bs
createSprites (b:bs) = [b-1, b, b+1] : createSprites bs

data Command = Noop | Addx Int
  deriving (Show)

calcCycles :: [Command] -> [Int]
calcCycles cs = helper [1] cs
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
