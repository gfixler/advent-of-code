module AOC08a where

import qualified Data.Map as M ((!), Map, elems, empty, insertWith, lookup)
import Debug.Trace

type Reg = String
type Comp = M.Map String Int

type Op = Int -> Int -> Int
type Instr = Int -> Int
type Check = Int -> Int -> Bool
type Cond = Comp -> Bool

type Cmd = (Reg, Instr, Cond)

readInt :: String -> Int
readInt = read

parseOp :: String -> Op
parseOp "inc" = flip (+)
parseOp "dec" = flip (-)

parseCond :: String -> Check
parseCond "<" = (<)
parseCond ">" = (>)
parseCond "<=" = (<=)
parseCond ">=" = (>=)
parseCond "==" = (==)
parseCond "!=" = (/=)

parse :: [String] -> Cmd
parse [r, op, n, _, r', c, n'] = (r, parseOp op (readInt n), chk)
    where chk comp = (parseCond c) (maybe 0 id (M.lookup r' comp)) (readInt n')

run :: Cmd -> Comp -> Comp
run (r, i, c) comp = M.insertWith (\n o -> if c' then i n else o) r (i 0) comp
    where c' = c comp

main :: IO ()
main = do
    cmds <- fmap (map (parse . words) . lines) $ readFile "input.txt"
    print $ maximum $ M.elems $ foldl (flip run) M.empty cmds

