module Main where

import Data.Sort

data Entry = Dir String [Entry] | File String Int deriving (Show)
type State = [Entry]

addEntry :: Entry -> Entry -> Entry
addEntry e (Dir n contents) = Dir n (e : contents)
addEntry _ _ = error "Can't add entry to a file"

doCmd :: State -> [String] -> State
doCmd (cwd:parent:rest) ["$", "cd", ".."] = addEntry cwd parent : rest
doCmd state ["$", "ls"] = state
doCmd state ["$", "cd", n] = Dir n [] : state
doCmd state ["dir", dir] = state
doCmd (cwd:rest) [size, n] = addEntry (File n (read size)) cwd : rest

walkDir :: Int -> Entry -> String
walkDir indent (Dir name contents) = (replicate indent ' ') ++ "- " ++ name ++ " (dir)\n" ++ (concatMap (walkDir (indent + 4)) contents)
walkDir indent (File name size) = (replicate indent ' ') ++ "- " ++ name ++ " (file, size=" ++ (show size) ++ ")\n"

dirSize :: Entry -> Int
dirSize (Dir _ contents) = sum $ map dirSize contents
dirSize (File _ size) = size

dirSizes :: Entry -> [Int]
dirSizes cwd@(Dir _ contents) = dirSize cwd : concatMap dirSizes contents
dirSizes (File _ size) = []

main = do
  content <- readFile "07-input.txt"
--  mapM_ putStrLn $ lines content

  let dirTree = foldl1 addEntry . foldl doCmd [] . map words . lines $ content

  putStrLn $ walkDir 0 dirTree

  print $ sum. filter (<100000) . dirSizes $ dirTree
  print $ head . filter (>= dirSize dirTree - 40000000) . sort . dirSizes $ dirTree