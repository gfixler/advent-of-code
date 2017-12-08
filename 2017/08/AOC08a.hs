module AOC08a where

import qualified Data.Map as M ((!), Map, elems, empty, insertWith, lookup)
import Debug.Trace

type Reg = String
type CPU = M.Map String Int

data Op = Inc | Dec deriving (Eq, Ord, Show)
data CondOp = OpLT | OpGT | OpLE | OpGE | OpEQ | OpNE deriving (Eq, Ord, Show)
type Cond = (Reg, CondOp, Int)
type Instr = (Reg, Op, Int)

readInt :: String -> Int
readInt = read

parseOp :: String -> Op
parseOp "inc" = Inc
parseOp "dec" = Dec

parseCond :: String -> CondOp
parseCond "<" = OpLT
parseCond ">" = OpGT
parseCond "<=" = OpLE
parseCond ">=" = OpGE
parseCond "==" = OpEQ
parseCond "!=" = OpNE

parseCondInstr :: [String] -> (Instr, Cond)
parseCondInstr [r1, o, n, _, r2, c, k] = ( (r1, parseOp o,   readInt n)
                                         , (r2, parseCond c, readInt k) )

getCond :: CondOp -> Int -> Int -> Bool
getCond OpLT = (<)
getCond OpGT = (>)
getCond OpLE = (<=)
getCond OpGE = (>=)
getCond OpEQ = (==)
getCond OpNE = (/=)

check :: CPU -> Cond -> Bool
check cpu (reg, cmp, n) = getCond cmp v n
    where v = maybe 0 id $ M.lookup reg cpu

update :: Instr -> CPU -> CPU
update (reg, op, n) = M.insertWith (+) reg (n * if op == Inc then 1 else -1)

run :: (Instr, Cond) -> CPU -> CPU
run (i, c) cpu = if check cpu c then update i cpu else cpu

main :: IO ()
main = do
    cmds <- fmap (map (parseCondInstr . words) . lines) $ readFile "input.txt"
    print $ maximum $ concatMap M.elems $ scanl (flip run) M.empty cmds

