module Day14 (solve) where

import Data.List.Split (splitOn)
import Utils (InputType (..), commonSolve)
import Data.List (nub)
import Data.Foldable (maximumBy)

solve :: IO ()
solve = commonSolve 14 Sample part1 part2

part1 input = rockCoordinates
  where
    rockCoordinates = Rock <$> nub (input >>= getRockCoordinates)

part2 :: b -> String
part2 = const "todo"

type Coordinate = (Int, Int)
data Obstacle = Rock Coordinate | Sand Coordinate deriving (Show)

coordinates :: Obstacle -> Coordinate
coordinates (Rock c) = c

yCoordinate :: Obstacle -> Int
yCoordinate = snd . coordinates

simulateSandFall :: [Obstacle] -> Coordinate -> Maybe Coordinate
simulateSandFall obstacles sandPosition
  | snd sandPosition > fallWhenY = Nothing
  | otherwise = Nothing
 where
   fallWhenY = maximum $ yCoordinate <$> obstacles
   obstacleOnCoordinate coordinate = 0

coordinateHasObstacle :: [Obstacle] -> Coordinate -> Bool
coordinateHasObstacle obstacles coordinate = any (\o -> coordinates o == coordinate) obstacles

parseLine :: String -> [Coordinate]
parseLine line = toPairs . toInts . splitOn "," <$> splitOn " -> " line
 where
  toPairs (a : b : _) = (a, b)
  toPairs l = error ("list length is not 2" <> show l)
  toInts cs = read <$> cs

getRockCoordinates :: String -> [Coordinate]
getRockCoordinates = nub . paths . parseLine
  where
    paths :: [Coordinate] -> [Coordinate]
    paths [] = []
    paths [c] = [c]
    paths (c1:c2:cs) = pathBetween c1 c2 ++ paths (c2:cs)

pathBetween :: Coordinate -> Coordinate -> [Coordinate]
pathBetween c1@(x1, y1) c2@(x2, y2)
  | c1 == c2 = [c1]
  | otherwise = c1 : pathBetween next c2
 where
  next
    | x1 < x2 = (x1 + 1, y1)
    | x1 > x2 = (x1 - 1, y1)
    | y1 < y2 = (x1, y1 + 1)
    | y1 > y2 = (x1, y1 - 1)
    | otherwise = error "should have ended"
