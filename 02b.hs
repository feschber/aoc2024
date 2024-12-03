incCond x y = x < y && abs (x - y) <= 3 && abs (x - y) > 0
decCond x y = x > y && abs (x - y) <= 3 && abs (x - y) > 0

-- l -> 'can remove' -> res
safeHInc :: Bool -> [Int] -> Bool
safeHInc r (x:y:xs) = incCond x y && safeHInc r (y:xs) || r && safeHInc False (x:xs)
safeHInc _ _ = True

safeHDec :: Bool -> [Int] -> Bool
safeHDec r (x:y:xs) = decCond x y && safeHDec r (y:xs) || r && safeHDec False (x:xs)
safeHDec _ _ = True

safe :: [Int] -> Bool
safe (x:xs) = safeHInc True (x:xs) || safeHDec True (x:xs) || safeHInc False xs || safeHDec False xs

main = getContents >>= putStrLn . show . sum . (map (\a -> if a then 1 else 0)) . (map safe) . (map ((map (\s -> read s :: Int)) . words)) . lines
