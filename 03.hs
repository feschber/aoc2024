import Text.Regex.TDFA

parse :: String -> [Int]
parse = (map read) . (\s -> getAllTextMatches (s =~ "[[:digit:]]+") :: [String])

main = do
  s <- getContents
  let res = getAllTextMatches (s =~ "mul\\([[:digit:]]+,[[:digit:]]+\\)") :: [String]
  let list = map parse res
  let s = (sum . (map product)) list
  putStrLn (show s)
