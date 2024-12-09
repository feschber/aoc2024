import Data.Array
import Data.List hiding (insert)
import Data.Maybe
import Data.Set (Set, empty, insert, member)

type A2 i e = Array (i, i) e

toArray :: [[e]] -> Array (Int, Int) e
toArray l = listArray ((0, 0), (length l - 1, length (head l) - 1)) (concat l)

parse :: String -> A2 Int Char
parse = toArray . lines

isGuard :: Char -> Bool
isGuard = (`elem` "<>^v")

data Dir = North | East | South | West

dir :: Char -> Dir
dir '^' = North
dir '>' = East
dir 'v' = South
dir '<' = West

rotate :: Char -> Char
rotate '^' = '>'
rotate '>' = 'v'
rotate 'v' = '<'
rotate '<' = '^'
rotate _ = error "not a guard"

nextPos :: (Num i) => (i, i) -> Dir -> (i, i)
nextPos (a, b) East = (a, b + 1)
nextPos (a, b) West = (a, b - 1)
nextPos (a, b) North = (a - 1, b)
nextPos (a, b) South = (a + 1, b)

findGuard :: (Ix i) => Array (i, i) Char -> Maybe (i, i)
findGuard = fmap fst . find (isGuard . snd) . assocs

inbounds :: (Ord i, Num i) => (i, i) -> (i, i) -> Bool
inbounds (a, b) (c, d) = 0 <= a && a <= c && 0 <= b && b <= d

step :: (Ix i, Num i, Ord i) => Array (i, i) Char -> (i, i) -> Maybe (Array (i, i) Char, (i, i))
step a p =
  let g = a ! p
      d = dir g
      np = nextPos p d
   in if inbounds np ((snd . bounds) a)
        then case a ! np of
          '#' -> step (a // [(p, rotate g)]) p
          _ -> Just (a // [(p, 'X'), (np, g)], np)
        else Nothing

printElem :: ((Int, Int), Char) -> IO ()
printElem ((_, 0), c) = putStrLn "" >> putChar c
printElem (_, c) = putChar c

printArray :: Array (Int, Int) Char -> IO ()
printArray a = mapM_ printElem (assocs a) >> putStrLn ""

allSteps :: (Ix i, Num i) => Array (i, i) Char -> (i, i) -> [(i, i)]
allSteps a g = g : case step a g of Nothing -> []; Just (b, n) -> allSteps b n

containsDuplicate :: (Ord e) => [e] -> Bool
containsDuplicate = containsDuplicateH empty

containsDuplicateH :: (Ord e) => Data.Set.Set e -> [e] -> Bool
containsDuplicateH _ [] = False
containsDuplicateH es (x : xs) = (x `member` es) || containsDuplicateH (insert x es) xs

hasLoop :: (Ix i, Num i) => Array (i, i) Char -> (i, i) -> Bool
hasLoop a g = (containsDuplicate . slidingWindow 2) (allSteps a g)

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 _ = Just []
takeMaybe _ [] = Nothing
takeMaybe n (x : xs) = (x :) <$> takeMaybe (n - 1) xs

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = mapMaybe (takeMaybe n) . tails

placeObstacle :: (Ix i) => Array (i, i) Char -> (i, i) -> Maybe (Array (i, i) Char)
placeObstacle a p = case a ! p of
  '#' -> Nothing
  '.' -> Just (a // [(p, '#')])
  _ -> Nothing

allObstaclePlacements :: (Ix i, Num i) => Array (i, i) Char -> [Array (i, i) Char]
allObstaclePlacements a = catMaybes (allObstaclePlacementsH a (0, 0))

allObstaclePlacementsH :: (Ix i, Num i) => Array (i, i) Char -> (i, i) -> [Maybe (Array (i, i) Char)]
allObstaclePlacementsH a p =
  if not (inbounds p ((snd . bounds) a))
    then []
    else
      let np = if snd p == (snd . snd . bounds) a then (fst p + 1, 0) else (fst p, snd p + 1)
       in placeObstacle a p : allObstaclePlacementsH a np

main :: IO ()
main = do
  f <- getContents
  let a = parse f
  let g = fromJust (findGuard a)
  let as = allObstaclePlacements a
  let loops = map (`hasLoop` g) as
  mapM_ (\(e, i) -> putStr (show i) >> putStr " has loop: " >> putStr (show e) >> putStrLn "") (zip loops [1 ..])
  let count = (length . filter id) loops
  -- mapM_ printArray loops
  -- print (length loops)
  print count
