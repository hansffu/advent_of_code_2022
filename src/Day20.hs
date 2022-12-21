module Day20 (solve) where

import Data.List.Extra (findIndex)
import Data.Maybe (fromMaybe)
import Utils (InputType (..), commonSolve, readInt, repeatList)

solve :: IO ()
solve = commonSolve 20 Input part1 part2

part1 :: [String] -> Int
part1 input = result !! 1000 + result !! 2000 + result !! 3000
 where
  orig = readInt <$> input
  mixed = repeatList $ mix orig 1
  result = dropWhile (/= 0) mixed

mix :: [Int] -> Int -> [Int]
mix original times = map fst $ iterate (\x -> foldl mix' x indexed) indexed !! times
 where
  indexed = zip original [0 ..]
  origLength = length original
  mix' :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
  mix' currentList (num, origIndex) =
    let currentIndex = fromMaybe 0 $ findIndex ((== origIndex) . snd) currentList
        nextIndexRaw = currentIndex + num
        newIndex = if nextIndexRaw == 0 then 6 else nextIndexRaw `mod` (origLength - 1)
     in move currentIndex newIndex currentList

move :: Int -> Int -> [a] -> [a]
move from to lst = left ++ middle ++ right
 where
  min' = min from to
  max' = max from to
  (left, rest) = splitAt min' lst
  (middle', right') = splitAt (max' - min' + if from > to then 0 else 1) rest
  (middle, right)
    | to == from = (middle', right')
    | from > to = (head right' : middle', tail right')
    | otherwise = (tail middle', head middle' : right')

part2 :: [String] -> Int
part2 input = result !! 1000 + result !! 2000 + result !! 3000
 where
  orig = (* 811589153) . readInt <$> input
  mixed = repeatList $ mix orig 10
  -- mixed = repeatList $ iterate mix orig !! 10
  result = dropWhile (/= 0) mixed
