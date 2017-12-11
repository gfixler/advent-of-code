module AOC11b where

import Control.Arrow (first, second)
import Data.Char
import Data.List.Split (splitOn)

type Pos = (Int, Int)

x = first
y = second

inc1 = (+1)
inc2 = (+2)
dec1 = subtract 1 -- cries
dec2 = subtract 2

data Dir = N | NE | SE | S | SW | NW deriving (Eq, Ord, Read, Show)

move :: Dir -> Pos -> Pos
move N  = y inc2
move NE = y inc1 . x inc1
move SE = y dec1 . x inc1
move S  = y dec2
move SW = y dec1 . x dec1
move NW = y inc1 . x dec1

origin :: Pos
origin = (0, 0)

parseInput :: String -> [Dir]
parseInput = map (read :: String -> Dir)
           . splitOn ","
           . map toUpper
           . filter (/= '\n')

hexTaxicab :: Pos -> Int
hexTaxicab (x, y) = diag + x'' + (y'' `div` 2)
    where (x', y') = (abs x, abs y)
          diag = min x' y'
          (x'', y'') = (x' - diag, y' - diag)

main :: IO ()
main = do
    s <- fmap parseInput $ readFile "input.txt"
    print $ maximum $ map hexTaxicab $ scanl (flip move) origin s

