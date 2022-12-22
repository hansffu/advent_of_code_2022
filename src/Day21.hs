module Day21 where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 21 Input part1 part2

part1 input = resolveValue (parseInput input) "root"

part2 input = tryHumn datamap 0
 where
  datamap = parseInput input

data Input = Num Int | Plus String String | Minus String String | Mul String String | Div String String deriving (Show)

type DataMap = Map.Map String Input

parseInput :: [String] -> DataMap
parseInput = foldr parseInput' Map.empty

parseInput' :: String -> DataMap -> DataMap
parseInput' input = Map.insert key parsed
 where
  elems = words input
  key = init $ head elems
  parsed = case elems of
    [_, a, "+", b] -> Plus a b
    [_, a, "-", b] -> Minus a b
    [_, a, "*", b] -> Mul a b
    [_, a, "/", b] -> Div a b
    [_, n] -> Num $ read n

-- otherwise -> error $ "Could not parse " <> input

resolveValue :: DataMap -> String -> Int
resolveValue datamap key =
  case Map.lookup key datamap of
    Nothing -> 0
    Just (Num n) -> n
    Just (Plus a b) -> resolve a + resolve b
    Just (Minus a b) -> resolve a - resolve b
    Just (Mul a b) -> resolve a * resolve b
    Just (Div a b) -> resolve a `div` resolve b
 where
  resolve = resolveValue datamap

resolveValue2 :: DataMap -> Int -> String -> Int
resolveValue2 datamap humnValue key
  | key == "humn" = humnValue
  | otherwise =
      case Map.lookup key datamap of
        Nothing -> 0
        Just (Num n) -> n
        Just (Plus a b) -> resolve a + resolve b
        Just (Minus a b) -> resolve a - resolve b
        Just (Mul a b) -> resolve a * resolve b
        Just (Div a b) -> resolve a `div` resolve b
 where
  resolve = resolveValue2 datamap humnValue

tryHumn :: DataMap -> Int -> Int
tryHumn datamap n
  | resolvedA == resolvedB = n
  | otherwise = tryHumn datamap (n + 1)
 where
  resolvedA = resolve rootA
  resolvedB = resolve rootB
  resolve = resolveValue2 datamap n
  (rootA, rootB) = case fromMaybe (error "no root") $ Map.lookup "root" datamap of
    Plus a b -> (a, b)
    Minus a b -> (a, b)
    Mul a b -> (a, b)
    Div a b -> (a, b)
    Num _ -> error "invalid root"
