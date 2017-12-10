module AOC10b where

import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.Map as M ((!), Map, fromList, insert, keys, toAscList, union)
import Data.List.Split (chunksOf, splitOn)

type Index = Int
type Length = Int
type KnotList a = M.Map Int a

type Cursor = Int
type Step = Int
type HashStep a = (Cursor, Step, KnotList a)

parseInput :: String -> [Int]
parseInput = map ord . filter (/= '\n')

getWrappedIndices :: Length -> Index -> Length -> [Int]
getWrappedIndices ll i l = map (`mod` ll) [i .. i + l - 1]

reverseSection :: Index -> Length -> KnotList a -> KnotList a
reverseSection i l m = M.union updates m
    where knotsCount = length $ M.keys m
          indices = getWrappedIndices knotsCount i l
          values = map (m M.!) indices
          updates = M.fromList $ zip indices (reverse values)

mapLength :: M.Map a b -> Int
mapLength = length . M.keys

knotHashStep :: HashStep a -> Length -> HashStep a
knotHashStep (c, s, k) l = ((c + l + s) `mod` mapLength k, s + 1, reverseSection c l k)

stdList :: KnotList Int
stdList = M.fromList $ zip is is
    where is = [0..255]

hexDigits :: [String]
hexDigits = (\a b -> [a, b]) <$> hs <*> hs
    where hs = "0123456789abcdef"

toHexDigit :: Int -> String
toHexDigit = (hexDigits !!)

main :: IO ()
main = do
    let suffix = [17, 31, 73, 47, 23]
    lengths <- fmap ((++ suffix) . parseInput) $ readFile "input.txt"
    let (c',s',m) = foldl knotHashStep (0, 0, stdList) (concat $ replicate 64 lengths)
        m' = map snd $ M.toAscList m
    print $ concatMap (toHexDigit . foldl1 xor) $ chunksOf 16 m'

