module Day9 (solve) where

import Data.List (nub)
import Utils (getInput)

solve :: IO ()
solve = do
  input <- getInput filename
  input2 <- getInput filename2
  putStrLn $ "Day 9:"
  putStrLn $ "Part 1:  " <> show (part1 input)
  putStrLn $ "Part 2:  " <> show (part2 input2)
  putStrLn "------------------------"
  where
    -- filename = "input/day9.sample.txt"
    -- filename2 = "input/day9-2.sample.txt"
    filename = "input/day9.input.txt"
    filename2 = "input/day9.input.txt"

data Direction = R | L | U | D
  deriving (Show)

type Input = (Direction, Int)
type Position = (Int, Int)

part1 :: [String] -> Int
part1 = length . nub . doMoves ((0, 0), [(0, 0)]) . parseInput

part2 :: [String] -> Int
part2  = length . nub . doMoves ((0, 0), replicate 9 (0, 0)) . parseInput

doMoves :: (Position, [Position]) -> [Direction] -> [Position]
doMoves _ [] = []
doMoves ((hx, hy), tails) (dir : ds) =
  let hpos = moveHead dir
      tpos = moveTail ( hpos, tails )
   in last tpos : doMoves (hpos, tpos) ds
 where
  -- ((hx, hy), (tx, ty)) = positions

  moveHead :: Direction -> Position
  moveHead R = (hx + 1, hy)
  moveHead L = (hx - 1, hy)
  moveHead U = (hx, hy + 1)
  moveHead D = (hx, hy - 1)

moveTail :: (Position, [Position]) -> [Position]
moveTail (_, []) = []
moveTail (parentPos, (tx, ty) : ts) = let pos =  follow parentPos  in pos : moveTail (pos, ts)
 where
  determineMoveInAxis :: Int -> Int -> Int -> Int
  determineMoveInAxis limit pc tc
    | pc - tc > limit = tc + 1
    | tc - pc > limit = tc - 1
    | otherwise = tc

  follow :: Position -> Position
  follow (x, y) =
    let diagonal = (abs (tx - x) + abs (ty - y)) > 2
        n = if diagonal then 0 else 1
     in (determineMoveInAxis n x tx, determineMoveInAxis n y ty)

parseInput :: [String] -> [Direction]
parseInput = flattenInput . map parseLine
 where
  parseLine ('R' : ' ' : num) = (R, read num)
  parseLine ('L' : ' ' : num) = (L, read num)
  parseLine ('U' : ' ' : num) = (U, read num)
  parseLine ('D' : ' ' : num) = (D, read num)
  parseLine input = error $ "Illegal input " <> input

  flattenInput :: [Input] -> [Direction]
  flattenInput [] = []
  flattenInput ((d, num) : xs) = replicate num d ++ flattenInput xs
