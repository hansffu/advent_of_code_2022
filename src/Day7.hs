module Day7 (solve) where

import Text.Regex.TDFA
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 7 Sample part1 part2

part1 :: [String] -> String
part1 input = show $ map parseLine input

data OutputLine
  = CdInto String
  | CdUp
  | Ls
  | DirOutput String
  | FileOutput String Int
  deriving (Show)

data FS = Dir [FS] | File Int


parseLine :: String -> OutputLine
parseLine c
  | isMatching cdUp = CdUp
  | isMatching cdBack = CdInto $ head $ getMatch cdBack
  | isMatching ls = Ls
  | isMatching dir = DirOutput $ head $ getMatch dir
  | isMatching file = let m = getMatch file in FileOutput (m !! 1) (read (head m) :: Int)
  | otherwise = error "unknown input"
  where
    cdUp = "cd .."
    cdBack = "cd (.*)"
    ls = "ls"
    dir = "dir (.*)"
    file = "([[:digit:]]+) (.*)"

    isMatching :: String -> Bool
    isMatching regex = c =~ regex

    getMatch :: String -> [String]
    getMatch regex = extractMatches (c =~ regex :: (String, String, String, [String]))

    extractMatches :: (String, String, String, [String]) -> [String]
    extractMatches (_, _, _, x) = x

-- where (c:cs) = input

part2 :: [String] -> String
part2 input = "TODO"
