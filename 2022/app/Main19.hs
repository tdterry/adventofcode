module Main where

import Data.Char
import Data.Sort
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List
import Data.List.Split
import Text.Regex.PCRE

data Distance = Dist Int | Infinity deriving (Show, Eq, Ord)
data Vertex = Vertex String Int deriving (Show, Eq, Ord)
data Edge = Edge String String Int deriving (Show, Eq, Ord)
data Graph = Graph [Vertex] [Edge] deriving (Show, Eq, Ord)
type CachedShortestPaths = M.Map (Vertex, Vertex) Distance
type CachedScore = M.Map Int Int -- step => score

data Inventory = Inventory { ore :: Int, clay :: Int, obsidian :: Int, geode :: Int } deriving (Show, Eq, Ord)
data Blueprint = Blueprint { num :: Int, oreCost :: Inventory, clayCost :: Inventory, obsidianCost :: Inventory, geodeCost :: Inventory } deriving (Show, Eq, Ord)

instance Num Distance where
    (+) (Dist a) (Dist b) = Dist (a + b)
    (+) _ _ = Infinity

instance Num Inventory where
    (+) (Inventory a b c d) (Inventory e f g h) = Inventory (a + e) (b + f) (c + g) (d + h)
    (-) (Inventory a b c d) (Inventory e f g h) = Inventory (a - e) (b - f) (c - g) (d - h)
--    (*) (Inventory a b c d) (Inventory e f g h) = Inventory (a * e) (b * f) (c * g) (d * h)
--    abs (Inventory a b c d) = Inventory (abs a) (abs b) (abs c) (abs d)
--    signum (Inventory a b c d) = Inventory (signum a) (signum b) (signum c) (signum d)
--    fromInteger a = Inventory (fromInteger a) (fromInteger a) (fromInteger a) (fromInteger a)

parseLine :: String -> Blueprint
parseLine s =
  let regex = "Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian."
      [[_, bp, ore, clay, obs1, obs2, geo1, geo2]] = s =~ regex :: [[String]]
  in Blueprint (read bp) (Inventory (read ore) 0 0 0) (Inventory (read clay) 0 0 0) (Inventory (read obs1) (read obs2) 0 0) (Inventory (read geo1) 0 (read geo2) 0)

canBuild :: Inventory -> Inventory -> Bool
canBuild (Inventory ore clay obs geode) (Inventory oreCost clayCost obsCost geodeCost) =
  ore >= oreCost && clay >= clayCost && obs >= obsCost && geode >= geodeCost

execute1 :: Blueprint -> (Inventory, Inventory) -> [(Inventory, Inventory)]
execute1 bp (materials, bots) =
  let materials' = materials + bots
      canGeode = canBuild materials' (geodeCost bp)
  in if canGeode
     then [(materials' - (geodeCost bp), bots + (Inventory 0 0 0 (num bp)))]
     else map (\(Just x) -> x) . filter (/=Nothing) $ [
       Just (materials', bots),
       if canBuild materials (oreCost bp)
         then Just (materials' - oreCost bp, bots + Inventory 1 0 0 0)
         else Nothing,
       if canBuild materials (clayCost bp)
         then Just (materials' - clayCost bp, bots + Inventory 0 1 0 0)
         else Nothing,
       if canBuild materials (obsidianCost bp)
         then Just (materials' - obsidianCost bp, bots + Inventory 0 0 1 0)
         else Nothing
     ]

executeN :: Blueprint -> Int -> [(Inventory, Inventory)] -> [(Inventory, Inventory)]
executeN bp 0 xs = xs
executeN bp n xs =
  let xs' = concatMap (execute1 bp) xs
  in executeN bp (n - 1) xs'

main :: IO ()
main =
  readFile "19-input-sample.txt" >>= \content ->

--  (mapM_ putStrLn (lines content)) >>

  mapM_ print (map parseLine . lines $ content) >>

  let bots = Inventory { ore = 1, clay = 0, obsidian = 0, geode = 0 }
      materials = Inventory { ore = 0, clay = 0, obsidian = 0, geode = 0 }
      blueprint = head . map parseLine . lines $ content
--      (Inventory ore clay obsidian geode) = foldl' (\i b -> i + (num b) * (oreCost b)) bots blueprints
  in print bots >>

  let start = [(materials, bots)]
--  in print (executeN blueprint 1 start) >>
--  print "" >>
--  print (executeN blueprint 2 start) >>
--  print "" >>
--  print (executeN blueprint 3 start) >>
--  print "" >>
--  print (executeN blueprint 4 start) >>
--  print "" >>
--  print (executeN blueprint 5 start) >>
--  print "" >>

      outcomes = executeN blueprint 20 start
  in mapM_ print (take 20 . sortBy (\(a,_) (b,_) -> compare (geode b) (geode a)) $ outcomes) >>

  return ()
