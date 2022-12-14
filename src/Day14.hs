module Day14 (solve) where

import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 14 Input part1 part2

type ObstacleSet = Set.Set Coordinate

part1 :: [String] -> Int
part1 input = simulate Nothing rockMap 0
 where
  rockMap = Set.fromList rockCoordinates
  rockCoordinates = nub (input >>= getRockCoordinates)

part2 :: [String] -> Int
part2 input = simulate caveFloor rockMap 0
 where
  rockMap = Set.fromList rockCoordinates
  rockCoordinates = nub (input >>= getRockCoordinates)
  caveFloor = Just $ 2 + maximum (yCoordinate <$> rockCoordinates)

type Coordinate = (Int, Int)

yCoordinate :: Coordinate -> Int
yCoordinate = snd

simulate :: Maybe Int -> ObstacleSet -> Int -> Int
simulate caveFloor obstacles counter = case sandPosition of
  Nothing -> counter
  Just pos -> simulate caveFloor (Set.insert pos obstacles) (succ counter)
 where
  sandPosition = simulateSandFall caveFloor obstacles (500, 0)

simulateSandFall :: Maybe Int -> ObstacleSet -> Coordinate -> Maybe Coordinate
simulateSandFall caveFloor obstacles sandPosition@(x, y)
  | isNothing caveFloor && snd sandPosition > fallWhenY = Nothing
  | isFree sandPosition = nextCoordinate
  | otherwise = Nothing
 where
  fallWhenY = maximum $ Set.map yCoordinate obstacles
  next = simulateSandFall caveFloor obstacles
  nextCoordinate
    | isFree (x, y + 1) = next (x, y + 1)
    | isFree (x - 1, y + 1) = next (x - 1, y + 1)
    | isFree (x + 1, y + 1) = next (x + 1, y + 1)
    | otherwise = Just sandPosition

  isFree = not . coordinateHasObstacle caveFloor obstacles

coordinateHasObstacle :: Maybe Int -> ObstacleSet -> Coordinate -> Bool
coordinateHasObstacle Nothing obstacles coordinate = coordinate `Set.member` obstacles
coordinateHasObstacle (Just caveFloor) obstacles coordinate = yCoordinate coordinate == caveFloor || coordinate `Set.member` obstacles

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
  paths (c1 : c2 : cs) = pathBetween c1 c2 ++ paths (c2 : cs)

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
