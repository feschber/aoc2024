module Main where

import Data.List

rotate :: Int -> [[a]] -> [[a]]
rotate 0 = id
rotate n = rotate (n - 1) . (reverse . transpose)

allLines :: [[a]] -> [[a]]
allLines m = concatMap (\n -> let m_ = rotate n m in (m_ ++ diagonals m_)) [0 .. 3]
  where
    diagonals =
      (++)
        <$> reverse . transpose . reverse . zipWith drop [1 ..] . reverse . transpose
        <*> transpose . zipWith drop [0 ..]

findXmas :: [Char] -> Int
findXmas [] = 0
findXmas ('X' : 'M' : 'A' : 'S' : xs) = 1 + findXmas xs
findXmas l = findXmas (tail l)

main :: IO ()
main = do
  i <- getContents
  let l = lines i
  let m = allLines l
  let c = (sum . map findXmas) m
  print (c :: Int)
