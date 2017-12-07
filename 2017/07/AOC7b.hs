module AOC7b where

type Name = String
type Weight = Int

data Tower = Tower Name Weight [Name] deriving Show

parse :: [String] -> Tower
parse (n:w:r) = Tower n (read $ init $ tail w) (f r)
    where f [] = []
          f (_:ts) = map init ts

main :: IO ()
main = do
    s <- readFile "input.txt"
    mapM_ print $ map (parse . words) $ lines s

