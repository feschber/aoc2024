import Data.List.Split
import Data.Maybe
import Data.Tuple
import Text.Read

main :: IO ()
main = getContents >>= print . sum . map fst . filter (uncurry isValid . swap) . parse

asdf :: String -> Maybe [Int]
asdf = mapM readMaybe . words

parseEquation :: String -> Maybe (Int, [Int])
parseEquation l = case splitOn ":" l of
  [a, b] -> liftA2 (,) (readMaybe a) (asdf b)
  _ -> Nothing

parse :: String -> [(Int, [Int])]
parse = fromJust . mapM parseEquation . lines

isValid :: [Int] -> Int -> Bool
isValid [] r = r == 0
isValid [a] r = r == a
isValid (x : y : xs) r = isValid ((x * y) : xs) r || isValid ((x + y) : xs) r
