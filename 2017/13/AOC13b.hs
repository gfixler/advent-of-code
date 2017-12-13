module AOC13a where

type Layer = Int
type Range = Int
type Pos   = Int
type Time  = Int
type Delay = Time

parseInput :: String -> (Int, Int)
parseInput s = case (words s) of
    [l, r] -> (read $ filter (/= ':') l, read r)
    _ -> undefined

scans :: (Layer, Range) -> [Pos]
scans (_, r) = cycle ([0 .. r-1] ++ [r-2, r-3 .. 1])

scan :: (Layer, Range) -> Time -> Pos
scan s = (scans s !!)

-- severity :: [(Layer, Range)] -> Delay -> Int
-- severity s t = sum
--              $ map ((\(l, r) -> l * r) . fst)
--              $ filter ((== 0) . snd)
--              $ zip s (map (flip scan t) s)

scanner :: (Layer, Range) -> Time -> String
scanner (l, r) t = concat [if i == scan (l, r) t then "[*]" else "[ ]" | i <- [0 .. r-1]]

main :: IO ()
main = do
    s <- fmap (map parseInput . lines) $ readFile "input.txt"
    -- let d = map (severity s) [0..]
    -- print $ length $ takeWhile (/= 0) d
    -- print $ d !! 22536
    -- print $ severity s 22536
    -- print $ map (flip scan 22536) s
    -- mapM_ print $ takeWhile (/= 0) $ d

