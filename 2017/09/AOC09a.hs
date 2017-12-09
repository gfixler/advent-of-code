module AOC09a where

import Data.List (group, unfoldr)
import Debug.Trace

type Level = Int
type Stream = String

data State = Collect
           | Garbage
           | Cancel
           deriving (Eq, Ord, Show)

type Cursor = (Level, State, Stream)

collectGroups :: Stream -> Stream
collectGroups "" = ""                       -- stop if done
collectGroups ('<':r) = bypassGarbage r     -- enter garbage mode
collectGroups (',':r) = collectGroups r     -- skip commas, they bore me
collectGroups (c:r) = c : collectGroups r   -- collect what made it through

bypassGarbage :: Stream -> Stream
bypassGarbage "" = ""                       -- stop if done
bypassGarbage ('!':"") = ""                 -- unlikely corner case
bypassGarbage ('!':c:r) = bypassGarbage r   -- skip character after '!'
bypassGarbage ('>':r) = collectGroups r     -- end garbage, back to collecting
bypassGarbage (c:r) = bypassGarbage r       -- do not keep garbage

-- countGroups :: Stream -> Int
-- countGroups = go (0, 0)
--     where go (l, _) "" = l
--           go (l, a) ('{':r) = let l' = l + 1 in go (l, a + l)
--           go (l, a) ('}':r) = go (l - 1, a)

main :: IO ()
main = do
    s <- readFile "input.txt"
    let g = collectGroups s
        n = foldl (\(l, a) c -> if c == '{' then (l + 1, a + l + 1) else (l - 1, a)) (0, 0) g
    print n

