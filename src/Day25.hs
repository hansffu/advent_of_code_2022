module Day25 (solve) where

import Utils (InputType (..), commonSolve, todo)

solve :: IO ()
solve = commonSolve 25 Input part1 todo

part1 :: [String] -> String
part1 input = intToSnafu $ sum $ snafuToInt <$> input

snafuToInt :: String -> Int
snafuToInt snafu = toInt $ reverse snafu
 where
  toInt :: String -> Int
  toInt [] = 0
  toInt [c] = snafuMapping c
  toInt (c : cs) = snafuMapping c + toInt cs * 5

snafuMapping :: Char -> Int
snafuMapping '0' = 0
snafuMapping '1' = 1
snafuMapping '2' = 2
snafuMapping '-' = -1
snafuMapping '=' = -2
snafuMapping _ = error "error"

intToSnafu :: Int -> String
intToSnafu 0 = "0"
intToSnafu 1 = "1"
intToSnafu 2 = "2"
intToSnafu d = case d `divMod` 5 of
  (c, 0) -> intToSnafu c <> "0"
  (c, 1) -> intToSnafu c <> "1"
  (c, 2) -> intToSnafu c <> "2"
  (c, 3) -> intToSnafu (c + 1) <> "="
  (c, 4) -> intToSnafu (c + 1) <> "-"
  _ -> error "impossible input"
