{-# LANGUAGE LambdaCase #-}

module AOC11a where

import Data.List (nub, sort)
import qualified Data.Map as M ((!), Map, fromList)

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

main :: IO ()
main = do
    m <- fmap parseInput $ readFile "input.txt"
    print $ sort $ findGroup m 0

