module Main where

import Data.Char
import Data.Sort
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List.Split

data Point = Point Int Int deriving (Show, Eq, Ord)

parsePair :: String -> Point
parsePair s = let [x,y] = map read . splitOn "," $ s in Point x y

parse :: String -> [[Point]]
parse = (map (map parsePair . (splitOn " -> "))) . lines

lerp :: Point -> Point -> [Point]
lerp (Point x1 y1) (Point x2 y2) = let
    dx = x2 - x1
    dy = y2 - y1
    steps = max (abs dx) (abs dy)
    in map (\i -> Point (x1 + (dx * i) `div` steps) (y1 + (dy * i) `div` steps)) [0..steps]

lerpPairs :: [Point] -> [Point]
lerpPairs (x:y:xs) = lerp x y ++ lerpPairs (y:xs)
lerpPairs _ = []

settle :: S.Set Point -> Int -> Bool -> Point -> (S.Set Point, Int)
settle walls maxY floor p =
  let (Point x y) = p
      down = Point x (y + 1)
      left = Point (x - 1) (y+1)
      right = Point (x + 1) (y+1)
  in
    if (S.member p walls) then (walls, 0)
    else if y > maxY then
      if floor then (S.insert p walls, 1)
      else (walls, 0)
    else if not (S.member down walls) then settle walls maxY floor down
    else if not (S.member left walls) then settle walls maxY floor left
    else if not (S.member right walls) then settle walls maxY floor right
    else (S.insert p walls, 1)

go :: S.Set Point -> Int -> Bool -> Int -> (S.Set Point, Int)
go walls maxY floor count =
  let (walls', c) = settle walls maxY floor (Point 500 0)
  in if c == 0 then (walls', count) else go walls' maxY floor (count+1)

runSim :: S.Set Point -> Bool -> Int
runSim walls floor =
  let maxY = maximum . map (\(Point _ y) -> y) . S.toList $ walls
      (_, count) = go walls maxY floor 0
  in count

main :: IO ()
main =
  readFile "14-input.txt" >>= \content ->

--  (mapM_ putStrLn (lines content)) >>

  let walls = S.fromList . concatMap lerpPairs $ parse content
  in return () >>

  print (runSim walls False) >>
  print (runSim walls True) >>

  return ()
