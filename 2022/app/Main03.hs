module Main where

import Data.Char
import Data.List

priority :: Char -> Int
priority x
  | x <= 'Z' = ord x - ord 'A' + 27
  | otherwise = ord x - ord 'a' + 1

badges :: [String] -> String
badges [] = ""
badges (x:y:z:xs) = (head . foldl1 intersect $ [x,y,z]) : badges xs

main = do
  content <- readFile "03-input.txt"
  let common = map (\x -> let (y,z) = splitAt (length x `div` 2) x in head (intersect y z)) . lines $ content
--  print common
  print . sum . map priority $ common


--  print . badges . lines $ content
  print . sum . map priority . badges . lines $ content
