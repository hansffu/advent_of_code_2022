module Day15 (solve) where


import Data.List(nub)
-- import Data.List.Utils (join)
import Utils (InputType (..), commonSolveIO, debug)

solve :: IO ()
solve = commonSolveIO 15 Sample part1 part2

part1 input = print $ length $ nub $ filter ((== 10) . snd) coveredLocations
-- part1 input = putStrLn $ join "\n" (draw coveredLocations)
  where
    -- scans = [parseLine  (input !! 6)]
    scans = parseLine <$> input
    beaconLocations = closestBeacon <$> scans
    coveredLocations = scans >>= coveredBy
    noProbeLocations = filter (\x -> x `notElem` beaconLocations) coveredLocations

part2 _ = putStrLn "todo"

type Coordinate = (Int, Int)

data Scan = Scan
  { sensorLocation :: Coordinate,
    closestBeacon :: Coordinate
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

coveredBy :: Scan -> [Coordinate]
coveredBy (Scan scanner@(x1, y1) probe) =
  [ (x, y)
    | x <- [minx .. maxx],
      y <- [miny .. maxy],
      distanceBetween scanner (x, y) <= distance
  ]
  where
    distance = distanceBetween scanner probe
    minx = x1 - distance
    maxx = x1 + distance
    miny = y1 - distance
    maxy = y1 + distance

draw :: [Coordinate] -> [String]
draw coordinates =
  [  [if (x, y) `elem` coordinates then '#' else '.' | x <- [-25 .. 25]] ++ show y
    | y <- [0 .. 25]
  ]
  where
    minx = minimum $ fst <$> coordinates
    maxx = maximum $ fst <$> coordinates
    miny = minimum $ snd <$> coordinates
    maxy = maximum $ snd <$> coordinates
