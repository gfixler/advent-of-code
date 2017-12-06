module AOC06b where

import Data.List (findIndex)
import Data.List.Split (chunksOf)
import Data.Set (Set, empty, fromList, insert)

data Split = Split [Int] Int [Int]

splitAtMax :: [Int] -> ([Int], Int, [Int])
splitAtMax xs = (take i xs, m, drop (i+1) xs)
   where m = maximum xs
         i = maybe undefined id $ findIndex (== m) xs

-- balance :: [Int] -> [Int]
balance xs = (take (length l) (drop (length r) xs''))
          ++ drop (length l + length r) xs''
          ++ take (length r) xs''
    where (l, m, r) = splitAtMax xs
          (q, re) = m `divMod` (length xs)
          xs' = map (+q) (r ++ l ++ [0])
          xs'' = zipWith (+) xs' (replicate re 1 ++ repeat 0)

main :: IO ()
main = do
    let banks = [5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6]
        result = scanl (flip insert) empty $ iterate balance banks
        compare = zipWith (==) result (tail result)
        firstloop = length $ takeWhile (/= True) compare
        result2 = scanl (flip insert) empty $ drop firstloop (iterate balance banks)
        compare2 = zipWith (==) result2 (tail result2)
    print $ length $ takeWhile (/= True) compare2

