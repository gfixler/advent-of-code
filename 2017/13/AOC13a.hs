module AOC13a where

type Layer = Int
type Range = Int
type Time  = Int
type Pos   = Int

parseInput :: String -> (Int, Int)
parseInput s = case (words s) of
    [l, r] -> (read $ filter (/= ':') l, read r)
    _ -> undefined

scan :: (Layer, Range) -> Time -> Pos
scan (l, r) = (cycle ([0 .. r-1] ++ [r-2, r-3 .. 1]) !!)

main :: IO ()
main = do
    s <- fmap (map parseInput . lines) $ readFile "input.txt"
    mapM_ print $ zip s (map (flip scan 0) s)

