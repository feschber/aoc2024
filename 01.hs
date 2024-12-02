import System.IO
import Data.List
import Data.Maybe

distance :: [Int] -> Maybe Int
distance [a, b] = Just (abs (a - b))
distance l = Nothing

main = getContents >>= putStrLn . show . sum . catMaybes . (map distance) . transpose . (map sort) . transpose . (map ((map read) . words)) . lines
