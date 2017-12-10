module AOC10a where

import qualified Data.Map as M ((!), Map, fromList, insert, keys, union)
import Data.List.Split (splitOn)

type Index = Int
type Length = Int
type KnotList = M.Map Int Int

readInt :: String -> Int
readInt = read

parseInput :: String -> [Int]
parseInput = map readInt . splitOn "," . filter (/= '\n')

getWrappedIndices :: Length -> Index -> Length -> [Int]
getWrappedIndices ll i l = map (`mod` ll) [i .. i + l - 1]

reverseSection :: Index -> Length -> KnotList -> KnotList
reverseSection i l m = M.union updates m
    where knotsCount = length $ M.keys m
          indices = getWrappedIndices knotsCount i l
          values = map (m M.!) indices
          updates = M.fromList $ zip indices (reverse values)

stdList :: KnotList
stdList = M.fromList $ zip is is
    where is = [0..255]

main :: IO ()
main = do
    s <- fmap parseInput $ readFile "input.txt"
    print $ reverseSection 0 189 stdList

