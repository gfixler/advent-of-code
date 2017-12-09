module AOC09b where

import Data.List (group, unfoldr)
import Debug.Trace

type Level = Int
type Stream = String

data State = Collect
           | Garbage
           | Cancel
           deriving (Eq, Ord, Show)

type Cursor = (Level, State, Stream)

collectGroups :: Stream -> [Int]
collectGroups [] = []                       -- stop if done
collectGroups ('<':r) = bypassGarbage r     -- enter garbage mode
collectGroups (c:r) = collectGroups r       -- toss non-garbage

bypassGarbage :: Stream -> [Int]
bypassGarbage [] = []                       -- stop if done
bypassGarbage ('!':[]) = []                 -- unlikely corner case
bypassGarbage ('!':c:r) = bypassGarbage r   -- skip canceled garbage
bypassGarbage ('>':r) = collectGroups r     -- end garbage, back to collecting
bypassGarbage (c:r) = 1 : bypassGarbage r   -- count garbage

main :: IO ()
main = do
    s <- readFile "input.txt"
    print $ sum $ collectGroups s

