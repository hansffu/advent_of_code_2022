module Day7 (solve) where

import Debug.Trace (traceShow)
import Text.Regex.TDFA
import Utils (InputType (..), commonSolve, debug)

solve :: IO ()
solve = commonSolve 7 Input part1 part2

part1 :: [String] -> String
part1 input = show $ dirsize $ head fs
 where
  commands = map parseLine input
  (fs, _) = getDirectoryContents commands

part2 :: [String] -> Int
part2 input = minimum $ filter (> toDelete) allDirs
 where
  diskSize = 70000000
  spaceNeeded = 30000000
  printsize :: FS -> (String, Int)
  printsize (File _ _) = ("file", 0)
  printsize dir@(Dir name _) = (name, filesize dir)
  commands = map parseLine input
  (fs, _) = getDirectoryContents commands
  allDirs = map filesize $ dirs $ head fs
  spaceUsed = maximum allDirs
  toDelete = spaceUsed - (diskSize - spaceNeeded)

data OutputLine
  = CdInto String
  | CdUp
  | Ls
  | DirOutput String
  | FileOutput String Int
  deriving (Show)

data FS = Dir String [FS] | File String Int
  deriving (Show)

dirs :: FS -> [FS]
dirs (File _ _) = []
dirs dir@(Dir _ []) = [dir]
dirs dir@(Dir _ files) = let subdirs = files >>= dirs in dir : subdirs

-- dirs dir@(Dir _ files) = dir : traceShow files []

dirsize :: FS -> Int
dirsize (File _ _) = 0
dirsize dir@(Dir _ files) =
  let s = filesize dir
      subdirsize = sum $ map dirsize files
   in if s < 100000
        then s + subdirsize
        else subdirsize

filesize :: FS -> Int
filesize (File _ size) = size
filesize (Dir _ files) = sum $ map filesize files

type Stack = [OutputLine]

getDirectoryContents :: Stack -> ([FS], Stack)
getDirectoryContents [] = ([], [])
getDirectoryContents ((FileOutput name size) : xs) = let (fs, rest) = getDirectoryContents xs in (File name size : fs, rest)
getDirectoryContents ((DirOutput _) : xs) = getDirectoryContents xs
getDirectoryContents (CdUp : xs) = ([], xs)
getDirectoryContents ((CdInto name) : xs) =
  let (filesInDir, restAfterSubdirs) = getDirectoryContents xs
      (fs, rest) = getDirectoryContents restAfterSubdirs
   in (Dir name filesInDir : fs, rest)
getDirectoryContents (x : xs) = getDirectoryContents xs

-- getDirectoryContents (CdInto _) = let

parseLine :: String -> OutputLine
parseLine c
  | isMatching dir = DirOutput $ head $ getMatch dir
  | isMatching file = let m = getMatch file in FileOutput (m !! 1) (read (head m) :: Int)
  | isMatching cdUp = CdUp
  | isMatching cdBack = CdInto $ head $ getMatch cdBack
  | isMatching ls = Ls
  | otherwise = error "unknown input"
 where
  cdUp = "cd \\.\\."
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
