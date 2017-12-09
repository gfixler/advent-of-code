module AOC09b where

type Stream = String

bypassGroups :: Stream -> [Int]
bypassGroups []      = []                   -- stop if done
bypassGroups ('<':r) = countGarbage r       -- enter garbage mode
bypassGroups (c:r)   = bypassGroups r       -- toss non-garbage

countGarbage :: Stream -> [Int]
countGarbage []        = []                 -- stop if done
countGarbage ('!':[])  = []                 -- unlikely corner case
countGarbage ('!':c:r) = countGarbage r     -- skip canceled garbage
countGarbage ('>':r)   = bypassGroups r     -- end garbage, back to collecting
countGarbage (c:r)     = 1 : countGarbage r -- count garbage

main :: IO ()
main = do
    s <- readFile "input.txt"
    print $ sum $ bypassGroups s

