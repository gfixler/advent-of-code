{-# LANGUAGE LambdaCase #-}

module AOC11b where

import Data.List (nub)
import qualified Data.Map as M ((!), Map, delete, fromList, null, keys)

type Prg = Int
type Net = M.Map Prg [Prg]

parseProg :: String -> (Prg, [Prg])
parseProg s = case words s of
    (p : "<->" : ps) -> (read p, map (read . filter (/= ',')) ps)

parseInput :: String -> Net
parseInput = M.fromList . map parseProg . lines

findGroup :: Net -> Prg -> [Prg]
findGroup m p = f ([], [p])
    where f (found, []) = found
          f (found, pending) = f (found ++ new, new)
            where new = nub $ filter (not . (`elem` found)) $ concatMap (m M.!) pending

toGroups :: Net -> [[Prg]]
toGroups m | M.null m = []
           | otherwise = g : toGroups m'
    where g = findGroup m $ head $ M.keys m
          m' = foldr M.delete m g

main :: IO ()
main = do
    m <- fmap parseInput $ readFile "input.txt"
    print $ length $ toGroups m

