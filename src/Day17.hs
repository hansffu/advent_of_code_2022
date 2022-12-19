module Day17 (solve) where

import Control.Monad (join)
import Data.List.Extra (chunksOf)
import qualified Data.String.Utils as StringUtils

-- import Debug.Trace (traceShow)
import Utils (InputType (..), commonSolveIO)


debug :: Show b => b -> b
debug = id

traceShow :: Show a => a -> b -> b
traceShow _ = id

solve :: IO ()
solve = commonSolveIO 17 Sample part1 part2

-- part1 input = putStrLn $ debugDraw result
part1 input = putStrLn $ show $ maximum $ snd . head <$> result
 where

  -- part1 input = putStrLn $ debugDraw result

  wind = repeatList $ head input
  ground = [[(x, 0)] | x <- [0 .. 6]]
  firstStonePosition = debug $ startingCoord ground Minus
  result = simulateFall firstStonePosition ground wind 2022

part2 input = putStrLn "todo"

data Stone = Minus | Plus | BackL | Pipe | Block deriving (Show, Enum)
type Coordinates = (Int, Int)
type Ground = [[Coordinates]]
type StonePosition = (Stone, Coordinates)
type Wind = String

repeatList :: [a] -> [a]
repeatList = join . repeat

-- Ground is flipped. (coordinate !! 3) is the third column. Ground floor is index 0
simulateFall :: StonePosition -> Ground -> Wind -> Int -> Ground
simulateFall _ ground [] _ = ground
simulateFall stonePosition@(stone, (x, y)) ground (wind : winds) roundsLeft
  | roundsLeft <= 0 = ground
  -- Hit ground and stopped
  | hitGround (traceShow ("down" <> show afterFall) afterFall) ground =
      simulateFall
        (nextStonePosition groundAfterFall stonePosition)
        groundAfterFall
        (wind : winds)
        (roundsLeft - 1)
  -- Hit sidewall. Ignore and continue
  | hitGround (traceShow ("wind " <> [wind] <> show afterWind) afterWind) ground =
      simulateFall
        afterFall
        ground
        winds
        (traceShow "sideways hit" roundsLeft)
  -- after wind and sideways move
  | otherwise = simulateFall afterWind ground winds roundsLeft
 where
  afterFall = (stone, (x, y - 1))
  afterWind' = case wind of
    '>' -> (stone, (x + 1, y - 1))
    '<' -> (stone, (x - 1, y - 1))
    _ -> error "illegal input"
  afterWind = if fst (snd afterWind') < 0 || (fst (snd afterWind') + stoneWidth stone) > 7 then afterFall else afterWind'
  groundAfterFall = addStoneToGround ground stonePosition

hitGround :: StonePosition -> Ground -> Bool
hitGround stonePosition@(stone, coordinate) ground = if result then traceShow ("hit ground " <> show hitGround') result else result
 where
  relevantColumns = take (stoneWidth stone) $ drop (fst coordinate) ground
  stoneCoords = stoneCoordinates stonePosition
  bottomOfStone = minimum $ snd <$> stoneCoordinates stonePosition
  relevantGround = relevantColumns >>= takeWhile (\(_, groundY) -> groundY >= bottomOfStone)
  hitGround' = filter (`elem` stoneCoords) relevantGround
  result = any (`elem` stoneCoords) relevantGround
  -- result = not $ null hitGround'

-- relevantGround = relevantColumns >>= takeWhile (`elem` stoneCoords)

nextStonePosition :: Ground -> StonePosition -> StonePosition
nextStonePosition ground (stone, _) = startingCoord ground nextStone
 where
  nextStone = succStone stone

addStoneToGround :: Ground -> StonePosition -> Ground
addStoneToGround g s = addS g $ stoneCoordinates s
 where
  addS :: Ground -> [Coordinates] -> Ground
  addS ground [] = ground
  addS ground ((x, y) : cs) = addS ([if i == x then (x, y) : (ground !! i) else ground !! i | i <- [0 .. 6]]) cs

succStone :: Stone -> Stone
succStone Block = Minus
succStone stone = succ stone

startingCoord :: Ground -> Stone -> StonePosition
startingCoord ground stone = traceShow ("before" <> show (stone, (2, highestGround + 5))) (stone, (2, highestGround + 5))
 where
  highestGround = maximum $ debug $ snd . head <$> ground

-- starting bottom left
stoneCoordinates :: StonePosition -> [Coordinates]
stoneCoordinates (stone, (x, y)) = stoneCoordinates' stone
 where
  stoneCoordinates' Minus = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]
  stoneCoordinates' Plus = [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 1, y + 2)]
  stoneCoordinates' BackL = [(x, y), (x + 1, y), (x + 2, y), (x + 2, y + 1), (x + 2, y + 2)]
  stoneCoordinates' Pipe = [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)]
  stoneCoordinates' Block = [(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)]

stoneHeight :: Stone -> Int
stoneHeight Minus = 1
stoneHeight Plus = 3
stoneHeight BackL = 3
stoneHeight Pipe = 4
stoneHeight Block = 2

stoneWidth :: Stone -> Int
stoneWidth Minus = 4
stoneWidth Plus = 3
stoneWidth BackL = 3
stoneWidth Pipe = 1
stoneWidth Block = 2

drawStone :: Stone -> IO ()
drawStone stone =
  putStrLn $
    StringUtils.join "\n" $
      reverse $
        chunksOf
          7
          ( [ if (x, y) `elem` coords then '#' else '.'
            | y <- [0 .. 6]
            , x <- [0 .. 6]
            ]
          )
 where
  coords = stoneCoordinates (stone, (0, 0))

debugDraw :: [[Coordinates]] -> String
debugDraw coordinates =
  ( StringUtils.join "\n" $
      reverse $
        zipWith (\i s -> if i == 0 then "+-------+" else "|" ++ s ++ "|" ++ show i) [0 ..] $
          chunksOf
            7
            ( [ if (x, y) `elem` coords then '#' else '.'
              | y <- [0 .. maxY]
              , x <- [0 .. 6]
              ]
            )
  )
 where
  coords = coordinates >>= id
  maxY = maximum $ snd <$> coords
