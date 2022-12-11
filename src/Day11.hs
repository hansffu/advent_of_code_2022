{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Day11 (solve) where

-- import Data.List (transpose)

import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (InputType (..), commonSolve, readInt)

solve :: IO ()
solve = commonSolve 11 Input part1 part2

part1 :: [String] -> Int
part1 input = product $ take 2 $ reverse $ sort handledByMonkey
  where
    monkeys = map parseMonkey $ splitOn [""] input
    initialItems = map ((0,) . startingItems) monkeys
    resultsAfter20 = executeRounds monkeys initialItems !! 19
    handledByMonkey = map fst resultsAfter20

part2 input = product $ take 2 $ reverse $ sort handledByMonkey
-- part2 input = a
  where
    a = product $ testIsDivisibleBy <$> monkeys
    monkeys = map parseMonkey $ splitOn [""] input
    initialItems = map ((0,) . startingItems) monkeys
    resultsAfter20 = executeRounds monkeys initialItems !! 9999
    handledByMonkey = map fst resultsAfter20

data Monkey = Monkey
  { monkeyId :: Int,
    startingItems :: [Int],
    operation :: Int -> Int,
    testIsDivisibleBy :: Int,
    trueToMonkey :: Int,
    falseToMonkey :: Int
  }

instance Show Monkey where
  show :: Monkey -> String
  show monkey = "Monkey: " <> show (monkeyId monkey) <> "  items: " <> show (startingItems monkey) <> show (operation monkey 1)

executeRounds :: [Monkey] -> [(Int, [Int])] -> [[(Int, [Int])]]
executeRounds monkeys initialMonkeyItems = roundResult : executeRounds monkeys roundResult
  where
    roundResult = executeRound monkeys initialMonkeyItems

executeRound :: [Monkey] -> [(Int, [Int])] -> [(Int, [Int])]
executeRound monkeys = execute 0
  where
    execute index allItems
      | index >= length monkeys = allItems
      | otherwise = execute (index + 1) $ executeThrow monkeys index allItems

executeThrow :: [Monkey] -> Int -> [(Int, [Int])] -> [(Int, [Int])]
executeThrow monkeys currentMonkeyIndex allMonkeyContAndItems = map (getMonkeyItems . monkeyId) monkeys
  where
    monkey = monkeys !! currentMonkeyIndex
    (count, items) = allMonkeyContAndItems !! currentMonkeyIndex
    throwResult = calculateThrows monkey items
    getMonkeyItems index =
      if index == currentMonkeyIndex
        then (count + length items, [])
        else (fst (allMonkeyContAndItems !! index), snd (allMonkeyContAndItems !! index) ++ map snd (filter (\x -> fst x == index) throwResult))

parseMonkey :: [String] -> Monkey
parseMonkey input =
  Monkey
    { monkeyId = readInt $ drop 7 $ init $ head input,
      startingItems = map readInt $ splitOn ", " $ drop 18 $ input !! 1,
      operation =
        let xs = words $ drop 23 $ input !! 2
         in createOperation xs,
      testIsDivisibleBy = readInt $ drop 21 $ input !! 3,
      trueToMonkey = readInt $ drop 29 $ input !! 4,
      falseToMonkey = readInt $ drop 30 $ input !! 5
    }

createOperation :: [String] -> Int -> Int
createOperation xs old = n
  where
    op = if head xs == "*" then (*) else (+)
    n = if last xs == "old" then op old old else op old (readInt $ last xs)

calculateThrows :: Monkey -> [Int] -> [(Int, Int)]
calculateThrows monkey = map calculateThrow
  where
    calculateThrow :: Int -> (Int, Int)
    calculateThrow item =
      let newWorryLevel = calculateWorryLevel monkey item
          testResult = (newWorryLevel `mod` testIsDivisibleBy monkey) == 0
          nextMonkey = (if testResult then trueToMonkey else falseToMonkey) monkey
       in (nextMonkey, newWorryLevel `mod` 9699690)

calculateWorryLevel :: Monkey -> Int -> Int
calculateWorryLevel monkey item = newItem `mod` modBy
 where
   modBy = 9699690
   newItem = operation monkey item
