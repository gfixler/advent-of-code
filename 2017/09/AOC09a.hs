module AOC09a where

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

main :: IO ()
main = do
    s <- readFile "input.txt"
    putStrLn $ collectGroups s

