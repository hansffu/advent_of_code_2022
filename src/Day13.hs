module Day13 (solve) where

import Control.Monad.State (State, evalState, state)
import Data.Char (digitToInt, isDigit)
import Data.List (sort, elemIndex)
import Data.List.Split (splitOn)
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 13 Input part1 part2

part1 :: [String] -> Int
part1 input = sum $ checkOrder 0 pairs
 where
  pairs = parsePair <$> splitOn [""] input
  isOrdered (left, right) = sort [left, right] == [left, right]
  checkOrder :: Int -> [(PacketData, PacketData)] -> [Int]
  checkOrder _ [] = []
  checkOrder index (x : xs) = (if isOrdered x then index + 1 else 0) : checkOrder (index + 1) xs

part2 :: [[Char]] -> Int
part2 input = index1 * index2
 where
  sorted = sort $ divider1 : divider2 : map parsePacket (filter (not . null) input)
  divider1 = parsePacket "[[2]]"
  divider2 = parsePacket "[[6]]"
  index1 = orElse  (succ <$> (divider1 `elemIndex` sorted))
  index2 = orElse  (succ <$> (divider2 `elemIndex` sorted))

orElse :: Maybe Int -> Int
orElse (Just i) = i
orElse Nothing = 0

data PacketData = PacketList [PacketData] | PacketNumber Int

instance Show PacketData where
  show (PacketNumber n) = show n
  show (PacketList l) = show l

instance Eq PacketData where
  PacketList l1 == PacketList l2 = l1 == l2
  PacketNumber n1 == PacketNumber n2 = n1 == n2
  PacketList l == PacketNumber n = PacketList l == PacketList [PacketNumber n]
  n@(PacketNumber _) == l@(PacketList _) = l == n

instance Ord PacketData where
  PacketList l1 <= PacketList l2 = l1 <= l2
  PacketNumber n1 <= PacketNumber n2 = n1 <= n2
  PacketList l <= PacketNumber n = PacketList l <= PacketList [PacketNumber n]
  PacketNumber n <= PacketList l = PacketList [PacketNumber n] <= PacketList l

parsePair :: [String] -> (PacketData, PacketData)
parsePair [] = error "illegal input"
parsePair [_] = error "illegal input"
parsePair (p1 : p2 : _) = (a, b)
 where
  a = parsePacket p1
  b = parsePacket p2

parsePacket :: String -> PacketData
parsePacket = head . evalState parseData

parseData :: State Stack [PacketData]
parseData = do
  nextChar <- pop
  case nextChar of
    Nothing -> return []
    Just c -> handleC c
 where
  handleC ',' = parseData
  handleC ']' = return []
  handleC '[' = do
    listContent <- parseData
    rest <- parseData
    return $ PacketList listContent : rest
  handleC num = do
    nextDigit <- popDigit
    let num' = case nextDigit of
          Just d -> digitToInt num * 10 + digitToInt d
          Nothing -> digitToInt num
    (:) (PacketNumber num') <$> parseData

type Stack = [Char]

pop :: State Stack (Maybe Char)
pop = state doPop
 where
  doPop :: [Char] -> (Maybe Char, [Char])
  doPop [] = (Nothing, [])
  doPop (x : xs) = (Just x, xs)

popDigit :: State Stack (Maybe Char)
popDigit = state doPop
 where
  doPop :: [Char] -> (Maybe Char, [Char])
  doPop [] = (Nothing, [])
  doPop (x : xs)
    | isDigit x = (Just x, xs)
    | otherwise = (Nothing, x : xs)
