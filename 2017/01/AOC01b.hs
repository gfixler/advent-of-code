module AOC01a where

import Data.Char (digitToInt)

fromDupe :: Eq a => (a, a) -> [a]
fromDupe (x, y) | x == y    = [x]
                | otherwise = []

main :: IO ()
main = do
    nums <- fmap (map digitToInt) $ readFile "input.txt"
    let hlen = length nums `div` 2
        rotl = drop hlen nums ++ take hlen nums
        keep = concatMap fromDupe $ zip nums rotl
    print $ sum keep

