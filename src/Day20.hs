module Day20 (solve) where


import Data.List.Extra (findIndex)

import Utils (InputType (..), commonSolve, readInt, debug)
import Data.Maybe (fromMaybe)

solve :: IO ()
solve = commonSolve 20 Sample part1 part2

part1 input = mix orig
 where
  orig = readInt <$> input

mix :: [Int] -> [Int]
mix original = map fst $ foldl mix' indexed indexed
 where
  indexed = zip original [0..]
  origLength = length original
  mix' :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
  mix' currentList item@(num, origIndex) =
    let currentIndex = fromMaybe 0 $  findIndex ((/= origIndex) . snd) currentList
        newIndex = (currentIndex + num) `mod` origLength
        (before, after') = splitAt  newIndex currentList
        after = if null after' then after' else tail after'
        -- before = takeWhile ((/= origIndex) . snd) currentList
        -- after = drop (length before) currentList
     in move currentIndex newIndex currentList

move :: Int -> Int -> [a] -> [a]
move from to lst = left ++ middle ++ right
 where
   (left, rest) = splitAt (min from to) lst
   (middle', right') = splitAt (max from to) lst
   (middle, right)
    | to == from = (middle', right')
    | to > from = (head right': middle' , tail right')
    | otherwise = (tail middle', head middle':right')


part2 :: [String] -> String
part2 _ = "todo"
