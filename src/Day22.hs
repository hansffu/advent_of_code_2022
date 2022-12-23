module Day22 (solve) where

import Data.Char (isDigit)
import Utils (InputType (..), commonSolve, readInt, todo)

solve :: IO ()
solve = commonSolve 22 Input part1 todo

part1 :: [String] -> Int
part1 input = 1000 * row + 4 * column + dirValue direction
 where
  (boardMap, instructions) = parseInput input
  padded = padBoard boardMap
  firstX = length $ takeWhile (/= Clear) $ head boardMap
  ((column, row), direction) = followInstructions padded instructions (firstX, 1) MoveRight

padBoard :: Map -> Map
padBoard board = topBottomRows : map padRow board ++ [topBottomRows]
 where
  maxX = maximum $ map (succ . length) board
  topBottomRows = replicate (maxX + 1) Outside
  padRow row =
    let rowLength = length row
     in [ if i == 0 || i > rowLength then Outside else row !! (i - 1)
        | i <- [0 .. maxX]
        ]

dirValue :: Direction -> Int
dirValue MoveRight = 0
dirValue MoveDown = 1
dirValue MoveLeft = 2
dirValue MoveUp = 3

data Tile = Outside | Clear | Wall deriving (Eq)
instance Show Tile where
  show Outside = "_"
  show Clear = "."
  show Wall = "#"

data Instruction = Move | TurnRight | TurnLeft deriving (Show, Eq)
data Direction = MoveUp | MoveRight | MoveDown | MoveLeft deriving (Show, Enum, Eq, Bounded)

type Map = [[Tile]]
type Coordinate = (Int, Int)

parseInput :: [String] -> (Map, [Instruction])
parseInput input = (mapInput, instructions)
 where
  mapInput = parseMapInput $ takeWhile (not . null) input
  instructions = parseInstructions $ last input

parseMapInput :: [String] -> Map
parseMapInput = map parseLine
 where
  parseLine = map parseTile
  parseTile '.' = Clear
  parseTile '#' = Wall
  parseTile ' ' = Outside
  parseTile c = error $ "invalid input: " <> show c

parseInstructions :: String -> [Instruction]
parseInstructions [] = []
parseInstructions ('R' : is) = TurnRight : parseInstructions is
parseInstructions ('L' : is) = TurnLeft : parseInstructions is
parseInstructions is = replicate num Move ++ parseInstructions (dropWhile isDigit is)
 where
  num = readInt digits
  digits = takeWhile isDigit is

followInstructions :: Map -> [Instruction] -> Coordinate -> Direction -> (Coordinate, Direction)
followInstructions _ [] coordinates direction = (coordinates, direction)
followInstructions boardMap (TurnLeft : is) coordinates direction = followInstructions boardMap is coordinates (predRound direction)
followInstructions boardMap (TurnRight : is) coordinates direction = followInstructions boardMap is coordinates (succRound direction)
followInstructions boardMap (Move : is) coordinates direction = followInstructions boardMap is finalPosition direction
 where
  (x, y) = coordinates
  idealPosition@(ix, iy) = case direction of
    MoveUp -> (x, y - 1)
    MoveRight -> (x + 1, y)
    MoveDown -> (x, y + 1)
    MoveLeft -> (x - 1, y)
  newPosition@(nx, ny)
    | isOutside =
        let column = map (!! ix) boardMap
            row = boardMap !! iy
         in case direction of
              MoveUp -> (x, length column - 1 - firstTileIndex (reverse column))
              MoveRight -> (firstTileIndex row, y)
              MoveDown -> (ix, firstTileIndex column)
              MoveLeft -> (length row - 1 - firstTileIndex (reverse row), iy)
    | otherwise = idealPosition
  finalPosition
    | isBlocked = coordinates
    | otherwise = newPosition
  isBlocked = lookupTile boardMap (nx, ny) == Wall
  isOutside = lookupTile boardMap (ix, iy) == Outside

firstTileIndex :: [Tile] -> Int
firstTileIndex [] = error "no tiles"
firstTileIndex (Outside : ts) = 1 + firstTileIndex ts
firstTileIndex _ = 0

succRound :: (Eq a, Bounded a, Enum a) => a -> a
succRound x
  | maxBound == x = minBound
  | otherwise = succ x

predRound :: (Eq a, Bounded a, Enum a) => a -> a
predRound x
  | minBound == x = maxBound
  | otherwise = pred x

lookupTile :: Map -> Coordinate -> Tile
lookupTile boardMap (x, y) = boardMap !! y !! x
