module AOC10a where

import qualified Data.Map as M ((!), Map, fromList, insert, keys, union)
import Data.List.Split (splitOn)

type Index = Int
type Length = Int
type KnotList = M.Map Int Int

type Cursor = Int
type Step = Int
type HashStep = (Cursor, Step, KnotList)

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

mapLength :: M.Map a b -> Int
mapLength = length . M.keys

knotHashStep :: HashStep -> Length -> HashStep
knotHashStep (c, s, k) l = ((c + l + s) `mod` mapLength k, s + 1, reverseSection c l k)

stdList :: KnotList
stdList = M.fromList $ zip is is
    where is = [0..255]

main :: IO ()
main = do
    lengths <- fmap parseInput $ readFile "input.txt"
    let (c',s',m) = foldl knotHashStep (0, 0, stdList) lengths
        elem1 = m M.! 0
        elem2 = m M.! 1
    print $ elem1 * elem2

