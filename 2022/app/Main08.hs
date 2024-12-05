module Main where

import qualified Data.Map as M

type Coords = (Int, Int)
type Forest = M.Map Coords Char

left (x, y) = (x - 1, y)
right (x, y) = (x + 1, y)
up (x, y) = (x, y - 1)
down (x, y) = (x, y + 1)

go :: Forest -> Coords -> (Coords -> Coords) -> [Char]
go f c d = map (f M.!) . takeWhile (flip M.member f) . iterate d $ d c

dirs = [left, right, up, down]

isVisible :: Forest -> (Coords, Char) -> Bool
isVisible f (c,tree) = any id . map (all (tree >) . go f c) $ dirs

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

score :: Forest -> (Coords, Char) -> Int
score f (c,tree) = product . map (length . takeUntil (tree <=) . go f c) $ dirs

main = do
  content <- readFile "08-input.txt"
--  mapM_ putStrLn $ lines content

  let forest = M.fromList $ zip [0..] (lines content) >>= \(y,l) -> zip [0..] l >>= \(x,c) -> return ((x,y),c)
--  print forest

  print $ length . filter (isVisible forest) . M.toList $ forest
  print $ maximum . map (score forest) . M.toList $ forest