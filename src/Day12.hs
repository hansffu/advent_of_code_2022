module Day12 where

import Utils (InputType (..), commonSolve)

import Data.Maybe (mapMaybe)

type Matrix = [[Char]]

solve :: IO ()
solve = commonSolve 12 Input part1 part2

-- part1 input = map neighbours $ neighbours (Node input 0 0 Nothing)
part1 input = shortestPath to from
 where
  from = Node input 0 0 Nothing
  to = Node input 5 2 Nothing

part2 _ = ""

data Node = Node Matrix Int Int (Maybe Node)

value :: Node -> Char
value (Node matrix x y _) = matrix !! y !! x

instance Show Node where
  -- show (Node matrix x y _) = "Node " <> [matrix !! y !! x] <> " (" <> show x <> "," <> show y <> ")"
  show node = [value node]

instance Eq Node where
  (Node _ x1 y1 _) == (Node _ x2 y2 _) = x1 == x2 && y1 == y2

neighbours :: Node -> [Node]
neighbours node@(Node matrix x y prev) = filter (\n -> notPrev n  && isValidJump n) $ left ++ right ++ up ++ down
 where
  notPrev = not . alreadyVisited node
  -- notPrev n = case prev of
  --   Just p -> p /= n
  --   Nothing -> True
  isValidJump nextNode =
    let curRaw = value node
        cur
          | curRaw == 'S' = 'a'
          | curRaw == 'E' = 'z'
          | otherwise = curRaw
        nextRaw = value nextNode
        next
          | nextRaw == 'S' = 'a'
          | nextRaw == 'E' = 'z'
          | otherwise = nextRaw
     in cur == next || succ cur == next

  left = [Node matrix (x - 1) y (Just node) | x > 0]
  right = [Node matrix (x + 1) y (Just node) | x < length (head matrix) - 1]
  up = [Node matrix x (y - 1) (Just node) | y > 0]
  down = [Node matrix x (y + 1) (Just node) | y < length matrix - 1]

shortestPath :: Node -> Node -> Maybe Int
shortestPath to from
  | from == to = Just 0
  | null neighbourLengths = Nothing
  | otherwise = Just $ succ $ minimum neighbourLengths -- \| otherwise = succ $ minimum $ shortestPath to <$> neighbours'
 where
  neighbours' = neighbours from
  neighbourLengths = mapMaybe (shortestPath to) (neighbours from)

alreadyVisited :: Node -> Node -> Bool
alreadyVisited (Node _ _ _ Nothing) _ = False
alreadyVisited (Node _ _ _ (Just prev)) next = if prev == next then True else alreadyVisited prev next
