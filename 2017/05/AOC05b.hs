module AOC05b where

import Data.List (unfoldr)
import qualified Data.Vector as V

numbers :: String -> IO [Int]
numbers = fmap (map read . lines) . readFile

type IndVec = (Int, V.Vector Int)

move :: IndVec -> Maybe (Int, IndVec)
move (i, v) = case v V.!? i of Nothing -> Nothing
                               Just o -> Just (i, (i+o, v V.// [(i, o + if o >= 3 then (-1) else 1)]))

main :: IO ()
main = do
    jumps <- numbers "input.txt"
    print $ length $ unfoldr move (0, V.fromList jumps)

