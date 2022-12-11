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
part1 input = executeGame monkeys 20 calculateWorryLevel1
  where
    monkeys = map parseMonkey $ splitOn [""] input

part2 :: [String] -> Int
part2 input = executeGame monkeys 10000 calculateWorryLevel
  where
    monkeys = map parseMonkey $ splitOn [""] input
    calculateWorryLevel = calculateWorryLevel2 $ product $ testIsDivisibleBy <$> monkeys

executeGame :: [Monkey] -> Int -> CalculateWorryLevel -> Int
executeGame monkeys rounds calculateWorryLevel = product $ take 2 $ reverse $ sort handledByMonkey
  where
    initialItems = map ((0,) . startingItems) monkeys
    resultsAfterRounds = executeRounds calculateWorryLevel monkeys initialItems !! (rounds - 1)
    handledByMonkey = map fst resultsAfterRounds

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

executeRounds :: CalculateWorryLevel -> [Monkey] -> [(Int, [Int])] -> [[(Int, [Int])]]
executeRounds calculateWorryLevel monkeys initialMonkeyItems = roundResult : executeRounds calculateWorryLevel monkeys roundResult
  where
    roundResult = executeRound calculateWorryLevel monkeys initialMonkeyItems

executeRound :: CalculateWorryLevel -> [Monkey] -> [(Int, [Int])] -> [(Int, [Int])]
executeRound calculateWorryLevel monkeys = execute 0
  where
    execute index allItems
      | index >= length monkeys = allItems
      | otherwise = execute (index + 1) $ executeThrow calculateWorryLevel monkeys index allItems

executeThrow :: CalculateWorryLevel -> [Monkey] -> Int -> [(Int, [Int])] -> [(Int, [Int])]
executeThrow calculateWorryLevel monkeys currentMonkeyIndex allMonkeyContAndItems = map (getMonkeyItems . monkeyId) monkeys
  where
    monkey = monkeys !! currentMonkeyIndex
    (count, items) = allMonkeyContAndItems !! currentMonkeyIndex
    throwResult = calculateThrows calculateWorryLevel monkey items
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

calculateThrows :: CalculateWorryLevel -> Monkey -> [Int] -> [(Int, Int)]
calculateThrows calculateWorryLevel monkey = map calculateThrow
  where
    calculateThrow :: Int -> (Int, Int)
    calculateThrow item =
      let newWorryLevel = calculateWorryLevel monkey item
          testResult = (newWorryLevel `mod` testIsDivisibleBy monkey) == 0
          nextMonkey = (if testResult then trueToMonkey else falseToMonkey) monkey
       in (nextMonkey, newWorryLevel `mod` 9699690)

type CalculateWorryLevel = Monkey -> Int -> Int

calculateWorryLevel1 :: CalculateWorryLevel
calculateWorryLevel1 monkey item = operation monkey item `div` 3

calculateWorryLevel2 :: Int -> CalculateWorryLevel
calculateWorryLevel2 modBy monkey item = newItem `mod` modBy
 where
   newItem = operation monkey item
