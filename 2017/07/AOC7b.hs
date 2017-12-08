module AOC7b where

import qualified Data.Map as M (Map, (!), elems, empty, fromList, insert, keys)
import Data.List (group, sort)

type Name = String
type Weight = Int

type Tower = (Weight, [Name])
type Towers = M.Map Name Tower

getTower :: Towers -> Name -> Tower
getTower = (M.!)

readTower :: [String] -> (Name, (Weight, [Name]))
readTower (n:w:r) = (n, (read $ init $ tail w, f r))
    where f [] = []
          f (_:ts) = map (filter (/= ',')) ts

getTowerChildren :: Towers -> Name -> [Name]
getTowerChildren = (snd .) . getTower

weigh :: Towers -> Name -> Weight
weigh ts n = w + sum (map (weigh ts) ts')
    where (w, ts') = getTower ts n

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame xs = all (== head (xs)) xs

noteUnbalance :: Towers -> Name -> String
noteUnbalance ts n = do
    let cs = getTowerChildren ts n
        ws = map (weigh ts) cs
        (w, _) = getTower ts n
    if (not (allSame ws)) then ("---> " ++ n ++ " ( " ++ show w ++ " ) :" ++ show (map (\x -> (x, weigh ts x)) (getTowerChildren ts n)))
                          else (n ++ " ( " ++ show w ++ " ) :" ++ show (map (\x -> (x, weigh ts x)) (getTowerChildren ts n)))

main :: IO ()
main = do
    s <- readFile "input.txt"
    let ts = M.fromList (map (readTower . words) $ lines s)
        ks = M.keys ts
        ws = unlines $ filter (not . null) $ map (noteUnbalance ts) ks
    putStrLn ws
    putStrLn $ "ihnus: " ++ show (weigh ts "ihnus")
    putStrLn $ "seeqikh: " ++ show (weigh ts "seeqikh")
    putStrLn $ "mqrfbqc: " ++ show (weigh ts "mqrfbqc")
    -- not the solution - I spelunked the noteUnbalance output and solved
    -- it manually :( (I also went 40 minutes over time).

