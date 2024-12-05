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
import Data.Maybe

data Distance = Dist Int | Infinity deriving (Show, Eq, Ord)
data Vertex = Vertex String Int deriving (Show, Eq, Ord)
data Edge = Edge String String Int deriving (Show, Eq, Ord)
data Graph = Graph [Vertex] [Edge] deriving (Eq, Ord)
type CachedShortestPaths = M.Map (Vertex, Vertex) Distance
type CachedScore = M.Map Int Int -- step => score

data Valve = Open Int | Closed Int deriving (Show, Eq, Ord)
type ValveStates = M.Map String Valve

instance Show Graph where
  show (Graph vs es) = "Graph\n  vertices=[\n" ++
    (foldl' (++) "" . map (\v -> "    " ++ show v ++ "\n") $ vs)
    ++ "  ]\n  edges=[\n" ++
    (foldl' (++) "" . map (\v -> "    " ++ show v ++ "\n") $ es)
    ++ "  ]"

instance Num Distance where
    (+) (Dist a) (Dist b) = Dist (a + b)
    (+) _ _ = Infinity

valveStates :: [Vertex] -> ValveStates
valveStates vs = M.fromList $ map (\(Vertex v r) -> (v, Closed r)) . filter (\(Vertex _ r) -> r /= 0) $ vs

data SearchState = SearchState (String, String, ValveStates, Int, Int, Int) deriving (Eq, Ord, Show) -- desc, vertex, valve states, steps, flow rate, total flow
data SearchState2 = None | SearchState2 (String, (String, String), ValveStates, Int, Int, Int, SearchState2) deriving (Eq, Ord, Show) -- desc, vertex, valve states, steps, flow rate, total flow, from
type BestStates = M.Map String SearchState
type BestStates2 = M.Map (String,String) SearchState2
type OpenStates = [SearchState]
type OpenStates2 = [SearchState2]

isBetter :: Int -> SearchState -> SearchState -> Bool
isBetter maxN (SearchState (_, _, _, xN, xRate, xFlow)) (SearchState (_, _, _, yN, yRate, yFlow)) =
  let xFuture = xFlow + xRate * (maxN - xN)
      yFuture = yFlow + yRate * (maxN - yN)
  in xFuture > yFuture

isBetter2 :: Int -> SearchState2 -> SearchState2 -> Bool
isBetter2 maxN (SearchState2 (_, _, _, xN, xRate, xFlow, _)) (SearchState2 (_, _, _, yN, yRate, yFlow, _)) =
  let xFuture = xFlow + xRate * (maxN - xN)
      yFuture = yFlow + yRate * (maxN - yN)
  in xFuture > yFuture

bestState :: Int -> [SearchState] -> SearchState
bestState maxN [x] = x
bestState maxN (x:xs) = foldl' (\a b -> if isBetter maxN a b then a else b) x xs

bestState2 :: Int -> [SearchState2] -> SearchState2
bestState2 maxN [x] = x
bestState2 maxN (x:xs) = foldl' (\a b -> if isBetter2 maxN a b then a else b) x xs

nextCandidates :: Graph -> SearchState -> [SearchState]
nextCandidates graph (SearchState (_, v, valveStates, n, flowRate, totalFlow))
  | n == 30 = []
  | all (\s -> case s of
      (Open _) -> True
      _ -> False) . M.elems $ valveStates = []
  | otherwise =
    let (Graph vs es) = graph
        nextVertices = map (\(Edge _ v2 _) -> v2) . filter (\(Edge v1 _ _) -> v1 == v) $ es
        nextStates = filter (\(SearchState(_, _, _, n, _, _)) -> n < 30) . map (\v -> SearchState ("move to", v, valveStates, n+1, flowRate, totalFlow+flowRate)) $ nextVertices
    in case M.lookup v valveStates of
      Just (Closed r) -> SearchState ("open valve", v, M.insert v (Open r) valveStates, n+1, flowRate+r, totalFlow+flowRate) : nextStates
      _ -> nextStates

nextCandidates2 :: Graph -> SearchState2 -> [SearchState2]
nextCandidates2 graph s@(SearchState2 (_, (v1, v2), valveStates, n, flowRate, totalFlow, _))
  | n == 26 = []
  | otherwise =
    let (Graph vs es) = graph
        myNextVertices = map (\(Edge _ v2 _) -> v2) . filter (\(Edge v _ _) -> v == v1) $ es
        elephantNextVertices = map (\(Edge _ v2 _) -> v2) . filter (\(Edge v _ _) -> v == v2) $ es
        nextStates = map (\v -> SearchState2 ("move,move", v, valveStates, n+1, flowRate, totalFlow+flowRate, s)) $ [(v1,v2) | v1 <- myNextVertices, v2 <- elephantNextVertices]
        myCanOpen = M.lookup v1 valveStates
        elephantCanOpen = M.lookup v2 valveStates
    in case (myCanOpen, elephantCanOpen) of
      (Nothing, Nothing) -> nextStates
      (Just (Closed r), Nothing) -> map (\v ->SearchState2 ("open,move", (v1, v), M.insert v1 (Open r) valveStates, n+1, flowRate+r, totalFlow+flowRate, s)) elephantNextVertices ++ nextStates
      (Nothing, Just (Closed r)) -> map (\v ->SearchState2 ("move,open", (v, v2), M.insert v2 (Open r) valveStates, n+1, flowRate+r, totalFlow+flowRate, s)) myNextVertices ++ nextStates
      (Just (Closed r1), Just (Closed r2)) ->
        map (\v ->SearchState2 ("open,move", (v1, v), M.insert v1 (Open r1) valveStates, n+1, flowRate+r1, totalFlow+flowRate, s)) elephantNextVertices ++
        map (\v ->SearchState2 ("move,open", (v, v2), M.insert v2 (Open r2) valveStates, n+1, flowRate+r2, totalFlow+flowRate, s)) myNextVertices ++
        if v1 /= v2 then [SearchState2 ("open,open", (v1, v2), M.insert v1 (Open r1) . M.insert v2 (Open r2) $ valveStates, n+1, flowRate+r1+r2, totalFlow+flowRate, s)] else [] ++
        nextStates
      _ -> nextStates

parseLine :: String -> (Vertex, [Edge])
parseLine s =
  let regex = "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? (.+)"
      [[_, n, r, ts]] = s =~ regex :: [[String]]
      vertex = Vertex n (read r)
      edges = map (\n2 -> Edge n n2 1) . (splitOn ", ") $ ts
  in (vertex, edges)


floydWarshall :: Graph -> Vertex -> Vertex -> [Vertex] -> Distance
floydWarshall g i j [] = Infinity
floydWarshall g i j (k:[]) =
  let (Graph _ edges) = g
      (Vertex from _) = i
      (Vertex to _) = j
  in if i == j then Dist 1
     else if (Edge from to 1) `elem` edges then Dist 1
     else Infinity
floydWarshall g i j (k:ks) =
  min (floydWarshall g i j ks) (floydWarshall g i k ks + floydWarshall g k j ks)

allShortestPaths :: Graph -> CachedShortestPaths
allShortestPaths g@(Graph vs _) =
  let pairs = [(i, j) | i <- vs, j <- vs]
  in M.fromList $ map (\v@(i, j) -> (v, floydWarshall g i j vs)) pairs

value :: Graph -> CachedShortestPaths -> Vertex -> Int -> Vertex -> Int
value g paths curr n target =
  let (Vertex _ r) = target
      d = paths M.! (curr, target)
  in case d of
    Infinity -> 0
    (Dist d') -> r * (n - d' - 1)

bestValue :: Graph -> CachedShortestPaths -> Vertex -> Int -> S.Set Vertex -> (Vertex, Int)
bestValue g paths curr n vs =
  let values = fmap (\v -> (v, value g paths curr n v)) . S.toList $ vs
      maxVal = head . reverse . sortOn snd $ values
  in maxVal

solve1 :: Graph -> [([String], Int)]
solve1 g@(Graph vs _) =
  let paths = allShortestPaths g
      verticesWithValves = S.fromList . filter (\(Vertex _ r) -> r > 0) $ vs
      startAt = head . S.toList $ verticesWithValves
      (v@(Vertex next _), value) = bestValue g paths startAt 30 verticesWithValves
  in [([next], value)]

flowAfter :: CachedShortestPaths -> Int -> [Vertex] -> Int
flowAfter m 0 _ = 0
flowAfter m n [] = 0
flowAfter m n (v:[]) = let (Vertex _ r) = v in r * (n-1)
flowAfter m n (v:vs) =
  let (Vertex _ r) = v
      (Vertex _ r2) = head vs
      (Dist d) = m M.! (v, head vs)
  in if r == 0
    then flowAfter m (n-d) vs
    else r * (n-1) + flowAfter m (n-1-d) vs

--solve1 :: Graph -> [([String], Int)]
--solve1 g =
--  let paths = allShortestPaths g
--      (Graph vs _) = g
--      verticesWithValves = filter (\(Vertex _ r) -> r > 0) $ vs
--      startAt = head . filter (\(Vertex v _) -> v == "AA") $ vs
--  in reverse . sortOn snd . map (\vs -> (map (\(Vertex v _) -> v) vs, flowAfter paths 30 vs)) . map (\vs -> startAt:vs). permutations $ verticesWithValves


printState :: Graph -> Int -> String -> SearchState -> IO ()
printState g depth indent state =
  let next = nextCandidates g state
  in putStrLn (indent ++ show state) >>
    if depth > 0
      then mapM_ (printState g (depth-1) (indent ++ "    ")) next
      else return ()

printState2 :: Graph -> Int -> String -> SearchState2 -> IO ()
printState2 g depth indent state =
  let next = nextCandidates2 g state
  in putStrLn (indent ++ show state) >>
    if depth > 0
      then mapM_ (printState2 g (depth-1) (indent ++ "    ")) next
      else return ()

solve' :: Graph -> OpenStates -> BestStates -> SearchState
solve' g s b
  | s == [] = bestState 30 . M.elems $ b
  | otherwise =
     let bestNext = head s
         (SearchState (_, v, _, _, _, _)) = bestNext
         currentBest = M.lookup v b
         s' = filter (/= bestNext) s
     in case currentBest of
       Nothing ->
         let b' = M.insert v bestNext b
             s'' = nub (s ++ nextCandidates g bestNext)
         in solve' g s'' b'
       (Just current) ->
         let b' = M.insert v bestNext b
             s'' = nub (s ++ nextCandidates g bestNext)
         in if isBetter 30 bestNext current
           then solve' g s'' b'
           else solve' g s' b

solve2' :: Graph -> OpenStates2 -> BestStates2 -> (BestStates2, SearchState2)
solve2' g s b
  | s == [] = (b, bestState2 24 . M.elems $ b)
  | otherwise =
     let bestNext = head s
         (SearchState2 (_, (v1,v2), _, _, _, _, _)) = bestNext
         currentBest = M.lookup (v1,v2) b
         s' = filter (/= bestNext) s
     in case currentBest of
       Nothing ->
         let b' = M.insert (v1,v2) bestNext b
             s'' = nub (s ++ nextCandidates2 g bestNext)
         in solve2' g s'' b'
       (Just current) ->
         let b' = M.insert (v1,v2) bestNext b
             s'' = nub (s ++ nextCandidates2 g bestNext)
         in if isBetter2 24 bestNext current
           then solve2' g s'' b'
           else solve2' g s' b


solve :: Graph -> SearchState
solve g =
  let (Graph vs _) = g
      valves =  valveStates vs
      start = SearchState ("start", "AA", valves, 0, 0, 0)
      openStates = [start]
  in solve' g openStates M.empty

solve2 :: Graph -> (BestStates2, SearchState2)
solve2 g =
  let (Graph vs _) = g
      valves =  valveStates vs
      start = SearchState2 ("start", ("AA","AA"), valves, 0, 0, 0, None)
      openStates = [start]
  in solve2' g openStates M.empty

path2 :: SearchState2 -> [String]
path2 (SearchState2 (desc, (v1,v2), _, n, r, t, from)) =
  let curr = desc ++ " -> " ++ show (v1,v2) ++ " n=" ++ show n ++ " r=" ++ show r ++ " t=" ++ show t
  in case from of
     None -> [curr]
     _ -> path2 from ++ [curr]

main :: IO ()
main =
  readFile "16-input-sample.txt" >>= \content ->

--  (mapM_ putStrLn (lines content)) >>

  let (nodes, edges) = unzip . map parseLine . lines $ content
      graph = Graph nodes (foldl1 (++) edges)
--      valves = valveStates nodes
--      startState = SearchState ("start", "AA", valves, 0, 0, 0)
--  in printState graph 2 "" startState >>
  in return () >>

  let (SearchState (_, _, _, n, r, t)) = solve graph
  in print ((30-n)*r + t) >>

  let (nodes, edges) = unzip . map parseLine . lines $ content
      graph = Graph nodes (foldl1 (++) edges)
--      valves = valveStates nodes
--      startState = SearchState2 ("start", ("AA","AA",False), valves, 0, 0, 0, ("","",False))
--  in printState2 graph 3 "" startState >>
  in return () >>

  let (b, solution) = solve2 graph
      SearchState2 (_, _, _, n, r, t, _) = solution
  in print ((26-n)*r + t) >>

  mapM_ print (path2 solution) >>

  return ()
