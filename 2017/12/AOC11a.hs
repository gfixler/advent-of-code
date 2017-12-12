{-# LANGUAGE LambdaCase #-}

module AOC11a where

import Data.List (nub)
import qualified Data.Map as M ((!), Map, fromList, keys)

type Prg = Int
type Net = M.Map Prg [Prg]

parseProg :: String -> (Prg, [Prg])
parseProg s = case words s of
    (p : "<->" : ps) -> (read p, map (read . filter (/= ',')) ps)

parseInput :: String -> Net
parseInput = M.fromList . map parseProg . lines

spider :: Net -> ([Prg], [Prg]) -> [Prg]
spider _ (old, [])   = old
spider n (old, new) = spider n (old ++ new', new')
    where new' = nub $ filter (not . (`elem` old)) $ concatMap (n M.!) new

main :: IO ()
main = do
    m <- fmap parseInput $ readFile "input.txt"
    print $ length $ spider m ([], [0])

