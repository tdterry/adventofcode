module Main where

import Data.List.Split
import Data.List

readInt :: String -> Maybe Int
readInt = read

main = do
  content <- readFile "01-input.txt"
  let elves = reverse . sort . map (sum . (map readInt) . splitOn "\n") . splitOn "\n\n" $ content
  print . head $ elves
  print . sum . take 3 $ elves