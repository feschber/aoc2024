module Main where

incCond :: (Ord a, Num a) => a -> a -> Bool
incCond x y = x < y && let d = abs (x - y) in 0 < d && d <= 3

-- l -> 'can remove' -> res
safeHInc :: Bool -> [Int] -> Bool
safeHInc r (x : y : xs) = incCond x y && safeHInc r (y : xs) || r && safeHInc False (x : xs)
safeHInc _ _ = True

safeH :: [Int] -> Bool
safeH l@(_ : xs) = safeHInc True l || safeHInc False xs
safeH _ = True

safe :: [Int] -> Bool
safe l = safeH l || safeH (reverse l)

parse :: String -> [[Int]]
parse = map (map (\s -> read s :: Int) . words) . lines

main :: IO ()
main = getContents >>= (print . length . filter id . map safe . parse)
