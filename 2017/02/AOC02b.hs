module AOC02b where

import Data.Char (digitToInt)
import Data.List (tails)

readString :: String -> Int
readString = read

isDivPair :: (Integral a, Ord a) => (a, a) -> Bool
isDivPair (x, y) = max x y `mod` min x y == 0

divPair :: Integral a => (a, a) -> a
divPair (x, y) = max x y `div` min x y

headAndTails :: [a] -> [(a, a)]
headAndTails [] = []
headAndTails [x] = []
headAndTails (x:xs) = zip (repeat x) xs

sumPair :: Num a => (a, a) -> a
sumPair (x, y) = x + y

solve xs = sum
         $ map ( divPair
               . head
               . filter isDivPair
               . concatMap headAndTails
               . tails
               . map readString
               . words )
         $ lines xs

main :: IO ()
main = do
    rows <- readFile "input.txt"
    print $ solve rows

