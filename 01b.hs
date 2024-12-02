import Data.List

occurrences :: [Int] -> Int -> Int
occurrences [] a = 0
occurrences (x:xs) a = (if x == a then 1 else 0) + occurrences xs a

takeTwo [a, b] = (a,b)
takeTwo _ = error "expected two lists"

mapOccurrences l1 l2 = map (\e -> e * (occurrences l2) e) l1

main = getContents >>= putStrLn . show . sum . (\ (l1, l2) -> mapOccurrences l1 l2) . takeTwo . transpose . (map ((map (\s -> read s :: Int)) . words)) . lines
