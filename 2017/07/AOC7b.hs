module AOC7b where

import qualified Data.Map as M (empty, fromList, insert)

type Name = String
type Weight = Int

parse :: [String] -> (Name, (Weight, [Name]))
parse (n:w:r) = (n, (read $ init $ tail w, f r))
    where f [] = []
          f (_:ts) = map init ts

main :: IO ()
main = do
    s <- readFile "input.txt"
    let m = M.fromList (map (parse . words) $ lines s)
    print m

