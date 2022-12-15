module Day15 (solve, solveSample) where

import Data.List (nub)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 15 Input (part1 2000000) part2

solveSample :: IO ()
solveSample = commonSolve 15 Sample (part1 10) part2

part1 :: Int -> [String] -> Int
part1 y input = length noBeaconLocations - scansOnY - beaconsOnY
 where
  scans = parseLine <$> input
  scansOnY = length $ nub $ filter ((== y) . snd) $ sensorLocation <$> scans
  beaconsOnY = length $ nub $ filter ((== y) . snd) $ closestBeacon <$> scans
  distance = maxDistance scans
  minX = minScanner scans - distance
  maxX = maxScanner scans + distance
  noBeaconLocations = filter (\x -> inRangeOfScans scans x y) [minX .. maxX]

part2 :: p -> String
part2 = const "todo"

inRangeOfScans :: [Scan] -> Int -> Int -> Bool
inRangeOfScans scans x y = any inRangeOfScan scans
 where
  inRangeOfScan :: Scan -> Bool
  inRangeOfScan (Scan scanner probe) = distanceBetween scanner (x, y) <= distanceBetween scanner probe

maxDistance :: [Scan] -> Int
maxDistance scanners = maximum $ map (\(Scan scanner probe) -> distanceBetween scanner probe) scanners

minScanner :: [Scan] -> Int
minScanner scanners = minimum $ map (fst . sensorLocation) scanners

maxScanner :: [Scan] -> Int
maxScanner scanners = maximum $ map (fst . sensorLocation) scanners

type Coordinate = (Int, Int)

data Scan = Scan
  { sensorLocation :: Coordinate
  , closestBeacon :: Coordinate
  }
  deriving (Show)

parseLine :: String -> Scan
parseLine input = Scan (x1, y1) (x2, y2)
 where
  w = words input
  x1 = read $ drop 2 $ init (w !! 2)
  y1 = read $ drop 2 $ init (w !! 3)
  x2 = read $ drop 2 $ init (w !! 8)
  y2 = read $ drop 2 (w !! 9)

distanceBetween :: Coordinate -> Coordinate -> Int
distanceBetween (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
