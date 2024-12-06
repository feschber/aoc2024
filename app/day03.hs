module Main where

import Text.Regex.TDFA

parse :: String -> [Int]
parse = map read . (\s -> getAllTextMatches (s =~ "[[:digit:]]+") :: [String])

main :: IO ()
main = do
  s <- getContents
  let res = getAllTextMatches (s =~ "mul\\([[:digit:]]+,[[:digit:]]+\\)") :: [String]
  let list = map parse res
  let r = (sum . map product) list
  print r
