import System.IO
import Data.List
import Data.Maybe

parseLine :: String -> [Int]
parseLine = (map read) . words

parseFile :: String -> [[Int]]
parseFile = (map parseLine) . lines

distance :: [Int] -> Maybe Int
distance [a, b] = Just (abs (a - b))
distance l = Nothing

main = getContents >>= putStrLn . show . sum . catMaybes . (map distance) . transpose . (map sort) . transpose . parseFile
