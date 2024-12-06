module Main where

import Data.List
import Data.Maybe

-- takes l1 and l2 and truncates l2 to the length of l1
truncateTo :: [a] -> [b] -> [b]
truncateTo = zipWith (\_ x -> x)

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 _ = Just []
takeMaybe _ [] = Nothing
takeMaybe n (x : xs) = (x :) <$> takeMaybe (n - 1) xs

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = mapMaybe (takeMaybe n) . tails

-- sliding window over multiple lists at once
slideParallel :: Int -> [[a]] -> [[[a]]]
slideParallel m = map transpose . slidingWindow m . transpose

slidingWindow2D :: Int -> Int -> [[a]] -> [[[a]]]
slidingWindow2D n m = concatMap (slideParallel m) . slidingWindow n

rotate :: Int -> [[a]] -> [[a]]
rotate 0 = id
rotate n = rotate (n - 1) . (reverse . transpose)

diagonal :: [[a]] -> [a]
diagonal = head . transpose . zipWith drop [0 ..]

allDiagonals :: [[a]] -> [[a]]
allDiagonals m = map (diagonal . (`rotate` m)) [0 .. 3]

matchXmas :: [String] -> Bool
matchXmas m = length ((filter (== "MAS") . allDiagonals) m) == 2

main :: IO ()
main = do
  i <- getContents
  let l = lines i
  let windows = slidingWindow2D 3 3 l
  let matches = (length . filter id . map matchXmas) windows
  print matches
