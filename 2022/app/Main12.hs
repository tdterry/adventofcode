module Main where

import Data.Char
import Data.Sort
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

type Node = (Int, Int)
data Distance a = Dist a | Infinity deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = False
  Infinity <= _ = False
  _ <= Infinity = True
  Dist a <= Dist b = a <= b

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist a) (Dist b) = Dist (a + b)
addDist _ _ = Infinity

type Grid = M.Map Node Int
type Distances = M.Map Node (Distance Int)
type Unvisited = S.Set Node

height :: Char -> Int
height c
  | c == 'S' = 0
  | c == 'E' = 25
  | otherwise = ord c - ord 'a'

neighbors :: Grid -> Node -> [Node]
neighbors g n@(x, y) = filter valid [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    valid n' = M.member n' g && (g M.! n) - (g M.! n') <= 1

findNext :: Unvisited -> Distances -> Node
findNext u = fst . head . sortOn snd . filter (\(k,_) -> S.member k u) . M.toList

solve :: Unvisited -> Distances -> Grid -> (Unvisited, Distances)
solve u d g
 | u == S.empty = (u, d)
 | otherwise =
     let n = findNext u d
         u' = S.delete n u
         newDist = addDist (d M.! n) (Dist 1)
         ns = neighbors g n
         d' = foldl (\d n' -> M.insert n' (min newDist (d M.! n')) d) d ns
     in solve u' d' g

main :: IO ()
main = do
  content <- readFile "12-input-alex.txt"
--  mapM_ putStrLn $ lines content

  let grid = concatMap (\(y, line) -> map (\(x, c) -> ((x, y), c)) $ zip [0..] line) $ zip [0..] $ lines content
--  print grid

  let start = fst . head . filter (\(_, c) -> c == 'S') $ grid
  let end = fst . head . filter (\(_, c) -> c == 'E') $ grid

  let heights = M.fromList $ fmap (fmap height) grid
--  print heights

  let unvisited = S.fromList $ M.keys heights
--  print unvisited

  let distances = M.insert end (Dist 0) . M.fromList . fmap (flip (,) (Infinity :: Distance Int)) $ M.keys heights
--  print distances

  let (_, distances') = solve unvisited distances heights



  print $ distances' M.! start
  print $ snd . head . sortOn snd . filter (\(k,_) -> heights M.! k == 0) . M.toList $ distances'