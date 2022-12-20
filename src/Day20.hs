module Day20 (solve) where


import Data.List.Extra (findIndex)

import Utils (InputType (..), commonSolve, readInt, debug)
import Data.Maybe (fromMaybe)

solve :: IO ()
solve = commonSolve 20 Sample part1 part2

part1 input = mix orig
 where
  orig = readInt <$> input
  origLength = length orig
  withIndex = zip [0 ..] orig

mix :: [Int] -> [Int]
mix original = map fst $ foldl mix' [] $ zip original [0 ..]
 where
  origLength = length original
  mix' :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
  mix' currentList item@(num, origIndex) =
    let currentIndex = fromMaybe 0 $  findIndex ((/= origIndex) . snd) currentList
        newIndex = (currentIndex + num) `mod` origLength
        (before, after') = splitAt  newIndex currentList
        after = if null after' then after' else tail after'
        -- before = takeWhile ((/= origIndex) . snd) currentList
        -- after = drop (length before) currentList
     in before ++ item : after

-- mix :: Int -> Int -> [Int] -> [Int]
-- mix startIndex end table
--  | startIndex == end = table
--  | otherwise = mixed
--   where
--     currentIndex =

part2 :: [String] -> String
part2 _ = "todo"
