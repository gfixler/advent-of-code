module AOC09a where

type Level  = Int
type Stream = String

collectGroups :: Stream -> Stream
collectGroups ""      = ""                  -- stop if done
collectGroups ('<':r) = bypassGarbage r     -- enter garbage mode
collectGroups (',':r) = collectGroups r     -- skip commas, they bore me
collectGroups (c:r)   = c : collectGroups r -- collect what made it through

bypassGarbage :: Stream -> Stream
bypassGarbage ""        = ""                -- stop if done
bypassGarbage ('!':"")  = ""                -- unlikely corner case
bypassGarbage ('!':c:r) = bypassGarbage r   -- skip character after '!'
bypassGarbage ('>':r)   = collectGroups r   -- end garbage, back to collecting
bypassGarbage (c:r)     = bypassGarbage r   -- do not keep garbage

countGroup :: (Level, Int) -> Char -> (Level, Int)
countGroup (l, a) '{' = (l + 1, a + l + 1)
countGroup (l, a) '}' = (l - 1, a)
countGroup li _ = li

main :: IO ()
main = do
    s <- readFile "input.txt"
    print $ foldl countGroup (0, 0) $ collectGroups s

