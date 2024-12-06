module Main where

import Data.List

twice f = f . f

rotate90 = reverse . transpose

rotate180 = twice rotate90

take_n n s l = take n (drop s l)

sliding_window n l = take (length l - n + 1) (zipWith (take_n n) [0 ..] (repeat l))

sl3 = sliding_window 3

sw2d3 = (map sl3) . sl3

main = do
  i <- getContents
  let l = lines i
  print (sw2d3 l)
