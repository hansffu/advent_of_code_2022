module Day15 (solve) where

import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 15 Sample part1 part2
part1 input = sensorLocations
 where
   sensorLocations = parseLine <$> input
part2 = const "todo"

type Coordinate = (Int, Int)
data Scan = Scan
  { sensorLocation :: Coordinate
  , closestBeacon :: Coordinate
  } deriving (Show)

parseLine :: String -> Scan
parseLine input = Scan (x1, y1) (x2, y2)
 where
   w = words input
   x1 = read $ drop 2 $ init (w !! 2)
   y1 = read $ drop 2 $ init (w !! 3)
   x2 = read $ drop 2 $ init (w !! 8)
   y2 = read $ drop 2 $ init (w !! 9)
