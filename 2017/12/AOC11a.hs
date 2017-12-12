{-# LANGUAGE LambdaCase #-}

module AOC11a where

import qualified Data.Set as S (Set, fromList)

parseProg :: String -> [(Int, Int)]
parseProg s = case words s of
    (p : "<->" : ps) -> let p' = read p
                            ps' = map (read . filter (/= ',')) ps
                            in [(min p' x, max p' x) | x <- ps']

parseInput :: String -> S.Set (Int, Int)
parseInput = S.fromList . concatMap parseProg . lines

main :: IO ()
main = do
    s <- fmap parseInput $ readFile "input.txt"
    print s

