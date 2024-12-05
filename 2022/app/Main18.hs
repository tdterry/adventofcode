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

data Cube = Cube Int Int Int deriving (Show, Eq, Ord)
data Side = PosX | NegX | PosY | NegY | PosZ | NegZ deriving (Show, Eq, Ord)

data CubeState = CubeState { cube :: Cube, sides :: S.Set Side } deriving (Show, Eq, Ord)
type Grid = S.Set Cube

ray :: (Cube, Cube) -> Cube -> Side -> [Cube]
ray (_, (Cube maxX _ _)) (Cube x y z) PosX = map (\x -> Cube x y z) [x+1..maxX]
ray ((Cube minX _ _), _) (Cube x y z) NegX = map (\x -> Cube x y z) [minX..x-1]
ray (_, (Cube _ maxY _)) (Cube x y z) PosY = map (\y -> Cube x y z) [y+1..maxY]
ray ((Cube _ minY _), _) (Cube x y z) NegY = map (\y -> Cube x y z) [minY..y-1]
ray (_, (Cube _ _ maxZ)) (Cube x y z) PosZ = map (\z -> Cube x y z) [z+1..maxZ]
ray ((Cube _ _ minZ), _) (Cube x y z) NegZ = map (\z -> Cube x y z) [minZ..z-1]

parseLine :: String -> CubeState
parseLine s =
  let [x,y,z] = splitOn "," s
      cube = Cube (read x) (read y) (read z)
      sides = S.fromList [PosX, NegX, PosY, NegY, PosZ, NegZ]
  in CubeState cube sides

unionCubes :: CubeState -> CubeState -> (CubeState, CubeState)
unionCubes a b =
  let cs1@(CubeState c1@(Cube x1 y1 z1) sides1) = a
      cs2@(CubeState c2@(Cube x2 y2 z2) sides2) = b
  in if x1 == x2 && y1 == y2 then
      if z1 == z2 + 1 then
        (CubeState c1 (S.delete PosZ sides1), CubeState c2 (S.delete NegZ sides2))
      else if z1 == z2 - 1 then
        (CubeState c1 (S.delete NegZ sides1), CubeState c2 (S.delete PosZ sides2))
      else (cs1, cs2)
    else if x1 == x2 && z1 == z2 then
      if y1 == y2 + 1 then
        (CubeState c1 (S.delete PosY sides1), CubeState c2 (S.delete NegY sides2))
      else if y1 == y2 - 1 then
        (CubeState c1 (S.delete NegY sides1), CubeState c2 (S.delete PosY sides2))
      else (cs1, cs2)
    else if y1 == y2 && z1 == z2 then
      if x1 == x2 + 1 then
        (CubeState c1 (S.delete PosX sides1), CubeState c2 (S.delete NegX sides2))
      else if x1 == x2 - 1 then
        (CubeState c1 (S.delete NegX sides1), CubeState c2 (S.delete PosX sides2))
      else (cs1, cs2)
    else (cs1, cs2)

reduceCubes :: [CubeState] -> CubeState -> [CubeState]
reduceCubes [] c = [c]
reduceCubes (x:xs) c =
  let (c', x') = unionCubes c x
  in x' : reduceCubes xs c'

findBounds :: [CubeState] -> (Cube, Cube)
findBounds [] = (Cube 0 0 0, Cube 0 0 0)
findBounds [x] = (cube x, cube x)
findBounds (x:xs) =
  let (Cube x1 y1 z1, Cube x2 y2 z2) = findBounds xs
      (Cube x3 y3 z3) = cube x
  in (Cube (min x1 x3) (min y1 y3) (min z1 z3), Cube (max x2 x3) (max y2 y3) (max z2 z3))

isInterior :: (Cube, Cube) -> Cube -> [Cube] -> Bool
isInterior bounds c grid =
  all id . map (\s -> any (`elem` grid) (ray bounds c s)) $ [PosX, NegX, PosY, NegY, PosZ, NegZ]

addInterior :: (Cube, Cube) -> [CubeState] -> [CubeState]
addInterior bounds grid =
  let (Cube minX minY minZ, Cube maxX maxY maxZ) = bounds
      cubes = [Cube x y z | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
      gridCubes = map cube grid
      interiorCubes = filter (\c -> not (c `elem` gridCubes) && (isInterior bounds c gridCubes)) cubes
  in foldl' reduceCubes grid (map (\c -> CubeState c (S.fromList [])) interiorCubes)

main :: IO ()
main =
  readFile "18-input.txt" >>= \content ->

--  mapM_ print (map parseLine . lines $ content) >>

  let cubes = map parseLine . lines $ content
  in mapM_ print cubes >>

  let cubes' = foldl' reduceCubes [] cubes
  in print (sum . map S.size . map sides $ cubes') >>

--  print cubes' >>

  let bounds = findBounds cubes'
      cubes'' = addInterior bounds cubes'
  in print (sum . map S.size . map sides $ cubes'') >>

--  print bounds >>
--  print cubes'' >>


  return ()
