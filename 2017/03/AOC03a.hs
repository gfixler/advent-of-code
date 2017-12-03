module AOC03a

import Control.Arrow (first, second)

spiralMoves :: Enum a => [(a, a) -> (a, a)]
spiralMoves = cycle [first succ, second succ, first pred, second pred]

doubleElems :: [a] -> [a]
doubleElems = concatMap (replicate 2)

stepFuncs :: [(Int, Int) -> (Int, Int)]
stepFuncs = concat $ zipWith replicate (doubleElems [1..]) spiralMoves

steps :: [(Int, Int)]
steps = (0, 0) : scanl (flip ($)) (0, 0) stepFuncs

spiralMemoryManhattanDist :: Int -> Int
spiralMemoryManhattanDist n | n <= 0 = undefined
                            | otherwise = let (h, v) = steps !! n in abs h + abs v

main :: IO ()
main = print $ spiralMemoryManhattanDist 361527

