module AOC04 where

import Data.List (nub, sort)

validPassphrase :: String -> Bool
validPassphrase s = let ss = map sort (words s) in ss == nub ss

solve :: String -> Int
solve = length . filter validPassphrase . lines

main :: IO ()
main = do
    inp <- readFile "input.txt"
    print $ solve inp

