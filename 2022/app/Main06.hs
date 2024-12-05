module Main where

import Data.List

isUnique :: String -> Bool
isUnique str = length str == length (nub str)

findCode :: Int -> String -> Int
findCode len str
  | isUnique $ take len str = len
  | otherwise = 1 + findCode len (tail str)

main = do
  content <- readFile "06-input.txt"
  print $ findCode 4 content
  print $ findCode 14 content
