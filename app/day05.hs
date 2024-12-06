module Main where

import Data.List.Split
import Data.Maybe
import Text.Read

parseRule :: String -> Maybe (Int, Int)
parseRule l = case splitOn "|" l of
  [a, b] -> liftA2 (,) (readMaybe a) (readMaybe b)
  _ -> Nothing

parseRules :: [String] -> ([(Int, Int)], [String])
parseRules [] = ([], [])
parseRules (x : xs) = case parseRule x of
  Just r -> let (l, rest) = parseRules xs in (r : l, rest)
  Nothing -> ([], x : xs)

parseUpdate :: String -> Maybe [Int]
parseUpdate = sequence . map readMaybe . splitOn ","

parseUpdates :: [String] -> [[Int]]
parseUpdates = mapMaybe parseUpdate

parse :: String -> ([(Int, Int)], [[Int]])
parse s =
  let l = lines s
   in let (rules, remaining) = parseRules l
       in let updates = parseUpdates (dropWhile (== "") remaining)
           in (rules, updates)

ruleApplies :: (Int, Int) -> [Int] -> Bool
ruleApplies _ [] = True
ruleApplies r@(a, b) (x : xs) = if a == x then True else if b == x then not (elem a xs) else ruleApplies r xs

isConform :: [(Int, Int)] -> [Int] -> Bool
isConform r u = all ((flip ruleApplies) u) r

middle :: [a] -> a
middle [a] = a
middle [a, _] = a
middle l = middle ((reverse . tail . reverse . tail) l)

main :: IO ()
main = do
  i <- getContents
  let (rules, updates) = parse i
  let conformUpdates = filter (isConform rules) updates
  let middles = map middle conformUpdates
  print (sum middles)
