windows :: [a] -> [(a, a)]
windows (x:y:z) = (x,y) : (windows (y:z))
windows _ = []

listPredicate :: (a -> a -> Bool) -> [a] -> Bool
listPredicate p = and . (map (\(a, b) -> p a b)) . windows

listPredicateAcc :: (a -> a -> Bool) -> [a] -> (a, Bool)
listPredicateAcc p (x:xs) = foldr (\curr -> \(prev, res) -> (curr, res && p prev curr)) (x, True) xs

distances = listPredicate (\a -> \b -> abs (a - b) <= 3 && abs (a - b) > 0)
inc = listPredicate (\a -> \b -> a < b)
dec = listPredicate (\a -> \b -> a > b)

safe l = (distances l) && (inc l || dec l)

main = getContents >>= putStrLn . show . sum . (map (\a -> if a then 1 else 0)). (map safe) . (map ((map (\s -> read s :: Int)) . words)) . lines
