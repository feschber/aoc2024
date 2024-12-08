import Data.Array
import Data.List
import Data.Maybe

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

-- printElem :: ((Int, Int), Char) -> IO ()
-- printElem ((_, 0), c) = putStrLn "" >> putChar c
-- printElem (_, c) = putChar c

-- printArray :: Array (Int, Int) Char -> IO ()
-- printArray a = mapM_ printElem (assocs a) >> putStrLn ""

allSteps :: (Ix i, Num i) => Array (i, i) Char -> (i, i) -> [(Array (i, i) Char, (i, i))]
allSteps a g = (a, g) : case step a g of Nothing -> []; Just (b, n) -> allSteps b n

main :: IO ()
main = do
  f <- getContents
  let a = parse f
  let g = fromJust (findGuard a)
  let (l, _) = last (allSteps a g)
  let count = (length . filter ((== 'X') . snd) . assocs) l
  print (count + 1)
