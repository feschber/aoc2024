module Main where

import Data.List

-- occurences of e in l
occurrences :: Int -> [Int] -> Int
occurrences e = length . filter (== e)

takeTwo :: [b] -> (b, b)
takeTwo [a, b] = (a, b)
takeTwo _ = error "expected two lists"

mapOccurrences :: [Int] -> [Int] -> [Int]
mapOccurrences l1 l2 = map (\e -> e * occurrences e l2) l1

parse :: String -> [[Int]]
parse = map (map (\s -> read s :: Int) . words) . lines

countOccurences :: [[Int]] -> Int
countOccurences = sum . uncurry mapOccurrences . takeTwo . transpose

main :: IO ()
main = getContents >>= (print . countOccurences . parse)
