module AOC13a where

type Layer = Int
type Range = Int
type Time  = Int
type Pos   = Int

parseInput :: String -> (Int, Int)
parseInput s = case (words s) of
    [l, r] -> (read $ filter (/= ':') l, read r)
    _ -> undefined

scan :: (Layer, Range) -> Pos
scan (l, r) = cycle ([0 .. r-1] ++ [r-2, r-3 .. 1]) !! l

main :: IO ()
main = do
    s <- fmap (map parseInput . lines) $ readFile "input.txt"
    print $ sum $ map ((\(l, r) -> l * r) . fst) $ filter ((== 0) . snd) $ zip s (map scan s)

