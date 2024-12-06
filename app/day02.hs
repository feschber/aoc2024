module Main where

safe :: [Int] -> Bool
safe l = distances l && (allWindows (<) l || allWindows (>) l)
  where
    windows l1 = l1 `zip` tail l1
    allWindows p = all (uncurry p) . windows
    distances = allWindows (\a b -> let d = abs (a - b) in d <= 3 && d > 0)

parse :: String -> [[Int]]
parse = map (map (\s -> read s :: Int) . words) . lines

main :: IO ()
main = getContents >>= print . length . filter id . map safe . parse
