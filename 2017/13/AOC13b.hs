module AOC13a where

type Layer = Int
type Range = Int
type Pos   = Int
type Delay = Int
type Program = (Layer, Range)

parseInput :: String -> (Int, Int)
parseInput s = case (words s) of
    [l, r] -> (read $ filter (/= ':') l, read r)
    _ -> undefined

caught :: Program -> Delay -> Bool
caught (l, r) d = (l - d) `mod` ((r - 1) * 2) == 0

run :: [Program] -> Delay -> Bool
run ps d = not $ all id $ map (flip caught d) ps

main :: IO ()
main = do
    s <- fmap (map parseInput . lines) $ readFile "input.txt"
    -- print $ length $ takeWhile id $ map (run s) [0..]
    print $ length $ takeWhile id $ map (run s) [0..]
