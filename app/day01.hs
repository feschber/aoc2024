module Main where

import Data.List
import Data.Maybe

distance :: [Int] -> Maybe Int
distance [a, b] = Just (abs (a - b))
distance _ = Nothing

main :: IO ()
main = getContents >>= print . sum . mapMaybe distance . transpose . map sort . transpose . map (map read . words) . lines
