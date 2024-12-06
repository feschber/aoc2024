import Data.List

twice f = f . f
rotate90 = reverse . transpose
rotate180 = twice rotate90

diagonals = (++) <$> reverse . transpose . reverse . zipWith drop [1..] . rotate180
                 <*> transpose . zipWith drop [0..]

all_lines m = m ++ diagonals m
both m = all_lines m ++ (all_lines . rotate90) m
total m = both m ++ (both . rotate180) m

find_xmas ('X' : 'M' : 'A' : 'S' : xs) = 1 + find_xmas xs
find_xmas (_:xs) = find_xmas xs
find_xmas [] = 0

main = do
  i <- getContents
  let l = lines i
  let m = total l
  let c = (sum . (map find_xmas)) m 
  print c
