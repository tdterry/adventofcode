module Main where

import Data.List.Split
import Data.Sort
import qualified Data.Map as M
import qualified Data.Set as S

data Monkey = Monkey { items :: [Integer], op :: (Input, String, Input), test :: Integer, targets :: (Integer, Integer), itemsInspected :: Integer } deriving (Show)
type Monkeys = M.Map Integer Monkey
data Input = Const Integer | Old deriving (Show)

parseStartLine :: String -> [Integer]
parseStartLine line = let [_,numbers] = splitOn ": " line
                      in map read (splitOn ", " numbers)

parseOpLine :: String -> (Input, String, Input)
parseOpLine line = let [_,op] = splitOn ": " line
                       [_, expr] = splitOn "= " op
                       [l, o, r] = words expr
                       first = if l == "old" then Old else Const (read l :: Integer)
                       second = if r == "old" then Old else Const (read r :: Integer)
                   in (first, o, second)

parseTestLine :: String -> Integer
parseTestLine line = let [_,test] = splitOn ": " line
                         [_, denom] = splitOn "divisible by " test
                     in read denom

parseTrueLine :: String -> Integer
parseTrueLine line = let [_,true] = splitOn "monkey " line
                     in read true

parseFalseLine :: String -> Integer
parseFalseLine line = let [_,false] = splitOn "monkey " line
                      in read false

parseMonkey :: String -> Monkey
parseMonkey s = let [_, startLine, opLine, testLine, trueLine, falseLine] = lines s
                    items = parseStartLine startLine
                    op = parseOpLine opLine
                    test = parseTestLine testLine
                    true = parseTrueLine trueLine
                    false = parseFalseLine falseLine
                in Monkey { items = items, op = op, test = test, targets = (false, true), itemsInspected = 0 }

opItem :: Integer -> (Input, String, Input) -> Integer
opItem item (l, o, r) = let left = case l of
                                        Old -> item
                                        Const i -> i
                            right = case r of
                                        Old -> item
                                        Const i -> i
                          in case o of
                               "+" -> left + right
                               "*" -> left * right
                               "-" -> left - right
                               "/" -> left `div` right

targetItem :: Integer -> Integer -> (Integer, Integer) -> Integer
targetItem item test (false, true) = if item `mod` test == 0 then true else false


doItem :: Integer -> Integer -> (Input, String, Input) -> Integer -> (Integer, Integer) -> Monkeys -> Monkeys
doItem denom item op test targets monkeys = let newItem = (opItem item op) `div` denom
                                                target = targetItem newItem test targets
                                      in M.adjust (\m -> m { items = items m ++ [newItem] }) target monkeys

doItem2 :: Integer -> Integer -> (Input, String, Input) -> Integer -> (Integer, Integer) -> Monkeys -> Monkeys
doItem2 denom item op test targets monkeys = let newItem = (opItem item op) `mod` denom
                                                 target = targetItem newItem test targets
                                      in M.adjust (\m -> m { items = items m ++ [newItem] }) target monkeys

doItems :: [Integer] -> (Input, String, Input) -> Integer -> (Integer, Integer) -> Monkeys -> Monkeys
doItems [] _ _ _ monkeys = monkeys
doItems (x:xs) op test targets monkeys = let newMonkeys = doItem 3 x op test targets monkeys
                                         in doItems xs op test targets newMonkeys

doItems2 :: Integer -> [Integer] -> (Input, String, Input) -> Integer -> (Integer, Integer) -> Monkeys -> Monkeys
doItems2 _ [] _ _ _ monkeys = monkeys
doItems2 denom (x:xs) op test targets monkeys = let newMonkeys = doItem2 denom x op test targets monkeys
                                         in doItems2 denom xs op test targets newMonkeys

stepMonkey :: Integer -> Monkeys -> Monkeys
stepMonkey n monkeys = let monkey = monkeys M.! n
                           itemCount = toInteger (length (items monkey))
                           newMonkeys = doItems (items monkey) (op monkey) (test monkey) (targets monkey) monkeys
                       in M.update (\_ -> Just Monkey{ items = [], op = (op monkey), test = (test monkey), targets = (targets monkey), itemsInspected = (itemsInspected monkey) + itemCount }) n newMonkeys

stepMonkey2 :: Integer -> Integer -> Monkeys -> Monkeys
stepMonkey2 denom n monkeys = let monkey = monkeys M.! n
                                  itemCount = toInteger (length (items monkey))
                                  newMonkeys = doItems2 denom (items monkey) (op monkey) (test monkey) (targets monkey) monkeys
                       in M.update (\_ -> Just Monkey{ items = [], op = (op monkey), test = (test monkey), targets = (targets monkey), itemsInspected = (itemsInspected monkey) + itemCount }) n newMonkeys

step :: Integer -> Monkeys -> Monkeys
step n monkeys
    | n >= toInteger (M.size monkeys) = monkeys
    | otherwise = step (n+1) (stepMonkey n monkeys)

step2 :: Integer -> Integer -> Monkeys -> Monkeys
step2 denom n monkeys
    | n >= toInteger (M.size monkeys) = monkeys
    | otherwise = step2 denom (n+1) (stepMonkey2 denom n monkeys)

main = do
  content <- readFile "11-input.txt"
--  mapM_ putStrLn $ lines content

  let monkeys = M.fromList $ zip[0..] (map parseMonkey (splitOn "\n\n" content))
--  print monkeys
  let state = head . drop 20 $ iterate (step 0) monkeys
  let (x:y:_) = reverse . sort $ map itemsInspected (M.elems state)
--  print state
  print (x * y)

  let lcm = product . map test $ M.elems monkeys
--  print lcm
  let state2 = head . drop 10000 $ iterate (step2 lcm 0) monkeys
  let (x2:y2:_) = reverse . sort $ map itemsInspected (M.elems state2)
--  print state2
--  print (x2, y2)
  print (x2 * y2)