module AOC02a where

readString :: String -> Int
readString = read

minMax :: Ord a => a -> (a, a) -> (a, a)
minMax x (y, z) = (min x y, max x z)

diff :: Num a => (a, a) -> a
diff (x, y) = abs (x - y)

solve xs = sum $ map (diff . (foldr minMax (maxBound, minBound)) . (map readString . words)) $ lines xs

main :: IO ()
main = do
    rows <- readFile "input.txt"
    print $ solve rows

