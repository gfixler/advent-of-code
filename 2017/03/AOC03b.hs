module AOC03b where

import Control.Arrow (first, second)
import Data.List (sort)
import Data.Maybe (mapMaybe)

import qualified Data.Map as M (Map, delete, fromList, insert, lookup, singleton, toList)

spiralMoves :: Enum a => [(a, a) -> (a, a)]
spiralMoves = cycle [first succ, second succ, first pred, second pred]

doubleElems :: [a] -> [a]
doubleElems = concatMap (replicate 2)

stepFuncs :: [(Int, Int) -> (Int, Int)]
stepFuncs = concat $ zipWith replicate (doubleElems [1..]) spiralMoves

steps :: [(Int, Int)]
steps = scanl (flip ($)) (0, 0) stepFuncs

lookAroundYou :: (Ord x, Ord y, Num x, Num y, Enum x, Enum y) =>
                 M.Map (x, y) Int -> (x, y) -> [Int]
lookAroundYou m (x, y) = mapMaybe (flip M.lookup m) cs
    where cs = filter (/= (x, y)) $ (,) <$> [x-1..x+1] <*> [y-1..y+1]

spiralAccum n = foldl
                (\m c -> M.insert c (sum (lookAroundYou m c)) m)
                (M.singleton (0,0) 1)
                (tail $ take n steps)

main :: IO ()
main = print $ last $ takeWhile (< input) values
    where input = 361527
          values = (sort . map snd . M.toList $ spiralAccum 100)

