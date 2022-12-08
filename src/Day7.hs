module Day7 (solve) where

import Control.Monad.State
import Text.Regex.TDFA
import Utils (InputType (..), commonSolve)

solve :: IO ()
solve = commonSolve 7 Input part1 part2

part1 :: [String] -> Int
part1 input = dirsize $ head fs
 where
  commands = map parseLine input
  (fs, _) = runState getDirC commands

part2 :: [String] -> Int
part2 input = minimum $ filter (> toDelete) allDirs
 where
  diskSize = 70000000
  spaceNeeded = 30000000
  commands = map parseLine input
  (fs, _) = runState getDirC commands
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

type Stack = [OutputLine]

pop :: State Stack (Maybe OutputLine)
pop = state doPop
 where
  doPop :: [OutputLine] -> (Maybe OutputLine, [OutputLine])
  doPop [] = (Nothing, [])
  doPop (x : xs) = (Just x, xs)

getDirC :: State Stack [FS]
getDirC = do
  input <- pop
  case input of
    Nothing -> return []
    (Just cmd) -> handleCmd cmd
 where
  handleCmd (FileOutput name size) = (:) (File name size) <$> getDirC
  handleCmd CdUp = return []
  handleCmd (CdInto name) = do
    dirContent <- getDirC
    rest <- getDirC
    return $ Dir name dirContent : rest
  handleCmd _ = getDirC
