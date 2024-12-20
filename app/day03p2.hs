module Main where

import Text.Regex.TDFA

parse :: String -> [Int]
parse = map read . (\s -> getAllTextMatches (s =~ "[[:digit:]]+") :: [String])

findMatches :: String -> [String]
findMatches s = getAllTextMatches (s =~ "mul\\([[:digit:]]+,[[:digit:]]+\\)") :: [String]

parseDo :: String -> [Char]
parseDo s = case (s =~ "do\\(\\)") :: (String, String, String) of
  (_, "", _) -> []
  (_, _, a) -> parseDont a

parseDont :: String -> [Char]
parseDont s = case (s =~ "don't\\(\\)") :: (String, String, String) of
  (b, "", _) -> b
  (b, _, a) -> b ++ parseDo a

main :: IO ()
main = do
  s <- getContents
  let l = parseDont s
  let matches = findMatches l
  let muls = map parse matches
  let products = map product muls
  let s_ = sum products
  print s_
