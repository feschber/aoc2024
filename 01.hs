import System.IO
import Data.List

parseLine :: String -> [Int]
parseLine = (map read) . words

parseFile :: String -> [[Int]]
parseFile = (map parseLine) . lines

distance :: [Int] -> Maybe Int
distance [a, b] = Just (abs (a - b))
distance l = Nothing

filterMap :: [Maybe a] -> [a]
filterMap [] = []
filterMap (Just a : xs) = a : filterMap xs
filterMap (Nothing : xs) = filterMap xs

main = getContents >>= putStrLn . show . sum . filterMap . (map distance) . transpose . (map sort) . transpose . parseFile
