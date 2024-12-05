module Main where

import Data.Char
import Data.Sort
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List.Split
import Text.Regex.PCRE

data Point = Point Int Int deriving (Show, Eq, Ord)
data Span = Span Int Int deriving (Show, Eq, Ord)
data Sensor = Sensor Point Point Int deriving (Show, Eq, Ord)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

canCombine :: Span -> Span -> Bool
canCombine (Span x1 x2) (Span y1 y2) = max x1 y1 <= min x2 y2 || (x2 + 1 == y1)

spanAt :: Int -> Sensor -> Maybe Span
spanAt y (Sensor (Point x1 y1) _ d) =
  let dy = abs (y-y1)
  in if dy > d
    then Nothing
    else Just $ Span (x1 - (d - dy)) (x1 + (d - dy))

parseLine :: String -> Sensor
parseLine s =
  let regex = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
      [[_, x1, y1, x2, y2]] = s =~ regex :: [[String]]
      sensor = Point (read x1) (read y1)
      beacon = Point (read x2) (read y2)
  in Sensor sensor beacon (manhattanDistance sensor beacon)

mapLine :: [Sensor] -> Int -> Int -> Int -> [Int]
mapLine g maxX y x
  | x > maxX = []
  | otherwise = (if any (\(Sensor s b d) -> manhattanDistance s (Point x y) <= d && (Point x y) /= b && (Point x y) /= s) g
    then []
    else [x]) ++ mapLine g maxX y (x + 1)

buildMap :: [Sensor] -> [Point] -> M.Map Point Char -> M.Map Point Char
buildMap _ [] m = m
buildMap sensors (p:xs) m =
  let c = head  (filter (\c -> c /= ' ') (map (\(Sensor s b d) -> if p == s then 'S'
      else if p == b then 'B'
      else if manhattanDistance s p <= d then '#'
      else ' ') sensors ++ "."))
  in buildMap sensors xs (M.insert p c m)

lineSpans :: [Sensor] -> Int -> [Span]
lineSpans sensors y =
  let spans = map (\(Just s) -> s) . filter (\s -> s /= Nothing) . map (spanAt y) $ sensors
  in sortOn (\(Span x _) -> x) spans

mergeSpans :: Span -> [Span] -> (Span, [Span])
mergeSpans s [] = (s, [])
mergeSpans s@(Span x1 x2) ss@(x@(Span y1 y2):xs) =
  if canCombine s x
  then mergeSpans (Span x1 (max x2 y2)) xs
  else (s, ss)

mergeAllSpans :: [Span] -> [Span]
mergeAllSpans [] = []
mergeAllSpans [x] = [x]
mergeAllSpans (x:xs) = let (merged, xs') = mergeSpans x xs
                  in merged : mergeAllSpans xs'

main :: IO ()
main =
  readFile "15-input.txt" >>= \content ->

--  (mapM_ putStrLn (lines content)) >>

  let g = map parseLine $ lines content
      minX = minimum $ map (\(Sensor (Point x _ ) _ d) -> x - d) g
      maxX = maximum $ map (\(Sensor (Point x _ ) _ d) -> x + d) g
      minY = minimum $ map (\(Sensor (Point _ y ) _ d) -> y - d) g
      maxY = maximum $ map (\(Sensor (Point _ y ) _ d) -> y + d) g
  in print g >>

  print (minX, maxX) >>

  let points = Point <$> [-10..30] <*> [-10..26]
      m = buildMap g points M.empty
  in mapM_ putStrLn (map (\y -> map (\x -> M.findWithDefault '.' (Point x y) m) [-4..26]) [0..20]) >>

  let spans = map (\l -> lineSpans g l) $ [0..4000000]
      merged = zip [0..4000000] $ map mergeAllSpans spans
  in mapM_ print (map (\(y,x) -> (x,y,4000000*x + y)) . map (\(y,((Span _ x):_)) -> (y,x+1)) . filter (\(l,ss) -> length ss == 2) $ merged) >>

--  print (lineSpans g 0) >>
--  print (lineSpans g 1) >>
--  print (lineSpans g 2) >>
--  print (lineSpans g 3) >>
--  print (lineSpans g 4) >>
--  print (lineSpans g 5) >>
--  print (lineSpans g 6) >>
--  print (lineSpans g 7) >>
--  print (lineSpans g 8) >>
--  print (lineSpans g 9) >>
--  print (lineSpans g 10) >>
--  print (lineSpans g 11) >>
--  print (lineSpans g 12) >>

--  mapM_ print (filter (\s -> let (Sensor (Point _ y) _ d) = s in abs(y-12) <= d) g) >>
--  print (lineSpans g 13) >>
--  print (lineSpans g 14) >>
--  print (lineSpans g 15) >>
--  print (lineSpans g 16) >>
--  print (lineSpans g 17) >>
--  print (lineSpans g 18) >>
--  print (lineSpans g 19) >>
--  print (lineSpans g 20) >>
--  print (lineSpans g 21) >>
--  print (lineSpans g 22) >>
--  print (lineSpans g 23) >>
--  print (lineSpans g 24) >>
--  print (lineSpans g 25) >>
--  print (lineSpans g 26) >>
--  print (lineSpans g 27) >>
--  print (lineSpans g 28) >>
--  print (lineSpans g 29) >>
--  print (lineSpans g 30) >>
--  print (lineSpans g 31) >>
--  print (lineSpans g 32) >>
--  print (lineSpans g 33) >>
--  print (lineSpans g 34) >>
--  print (lineSpans g 35) >>
--  print (lineSpans g 36) >>
--  print (lineSpans g 37) >>
--  print (lineSpans g 38) >>
--  print (lineSpans g 39) >>
--  print (lineSpans g 40) >>
--  print (map (lineSpans g) [0..20]) >>

--  let c' = findBeacon g 20 20 0
--  in print c' >>

  return ()
