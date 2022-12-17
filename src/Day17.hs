module Day17 where

import Control.Monad (join)
import Data.Bitraversable (Bitraversable)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 17 Sample part1 part2

part1 input = take 80 wind
 where
  wind = repeatList $ head input

part2 input = "todo"

data Stone = Minus | Plus | BackL | Pipe | Block deriving (Show, Enum)
type Coordinates = (Int, Int)
type Ground = [[Coordinates]]
type StonePosition = (Stone, Coordinates)
type Wind = String

stoneList :: [Stone]
stoneList = repeatList $ enumFrom Plus

repeatList :: [a] -> [a]
repeatList = join . repeat


-- Ground is flipped. (coordinate !! 3) is the third column. Ground floor is index 0
simulateFall :: StonePosition -> Ground -> Wind -> Int -> Ground
simulateFall _ ground [] _ = ground
simulateFall stonePosition@(stone, (x,y)) ground (wind:winds) roundsLeft
  | hitGround stonePosition ground = ground
 where
   afterFall = stoneCoordinates (stone, (x, y+1))

hitGround :: StonePosition -> Ground -> Bool
hitGround (stone, coordinate) ground = True

stoneCoordinates :: StonePosition -> [Coordinates]
stoneCoordinates (stone, (x, y)) = stoneCoordinates' stone
 where
  stoneCoordinates' Plus = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)] -- starting in the middle
  stoneCoordinates' Minus = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] -- starting left
  stoneCoordinates' BackL = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] -- starting bottom right
  stoneCoordinates' Pipe = [(x,y), (x, y+1), (x, y+2), (x, y+3)] -- starting bottom
  stoneCoordinates' Block = [(x,y), (x+1, y), (x, y+1), (x+1, y+1)] -- starting bottom left
