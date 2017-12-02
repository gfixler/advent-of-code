module AOC01a where

import Data.Char (digitToInt)

fromDupe :: Eq a => (a, a) -> [a]
fromDupe (x, y) | x == y    = [x]
                | otherwise = []

main :: IO ()
main = do
    nums <- fmap (map digitToInt) $ readFile "input.txt"
    let keep = concatMap fromDupe $ zip nums (tail nums)
        wrap = if (head nums) == (last nums) then head nums else 0
    print $ sum keep + wrap

