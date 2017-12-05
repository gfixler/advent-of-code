module AOC04a where

import Data.List (nub)

validPassphrase :: String -> Bool
validPassphrase s = let ss = words s in ss == nub ss

solve :: String -> Int
solve = length . filter validPassphrase . lines

main :: IO ()
main = do
    inp <- readFile "input.txt"
    print $ solve inp

