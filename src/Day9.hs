module Day9 (solve) where

import Data.List (nub)
import Utils (InputType (..), commonSolve, debug)

solve :: IO ()
solve = commonSolve 9 Input part1 part2

data Direction = R | L | U | D
  deriving (Show)

type Input = (Direction, Int)
type Position = (Int, Int)

part1 :: [String] -> Int
part1 = length . nub . doMoves ((0, 0), (0, 0)) . parseInput

part2 _ = "todo"

doMoves :: (Position, Position) -> [Direction] -> [Position]
doMoves _ [] = []
doMoves positions (dir : ds) =
  let hpos = moveHead dir
      tpos = follow hpos
   in tpos : doMoves (hpos, tpos) ds
 where
  ((hx, hy), (tx, ty)) = positions

  moveHead :: Direction -> Position
  moveHead R = (hx + 1, hy)
  moveHead L = (hx - 1, hy)
  moveHead U = (hx, hy + 1)
  moveHead D = (hx, hy - 1)

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
