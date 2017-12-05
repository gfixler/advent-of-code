module AOC05a where

readNum :: (Num a, Read a) => String -> a
readNum = read

data Action = Ready | SteppingL Int | SteppingR Int | Done deriving (Eq, Ord, Show)
data Instr = Blank | Jump Int deriving (Eq, Ord, Show)
data Maze = Maze (Instr, ([Instr], [Instr]))
data Moment = Moment (Action, Maze) deriving Show

instance Show Maze where
    show (Maze (e, (l, r))) = (show (reverse $ take 3 l))
                           ++ " [ "
                           ++ (show e)
                           ++ " ] "
                           ++ (show (take 3 r))

moveL :: Maze -> Maze
moveL (Maze (o, (l:ls, rs))) = Maze (l, (ls, o:rs))

moveR :: Maze -> Maze
moveR (Maze (o, (ls, r:rs))) = Maze (r, (o:ls, rs))

move :: Moment -> Moment
move (Moment (Ready, z@(Maze (Blank, _)))) = Moment (Done, z)
move (Moment (Ready, Maze (Jump o, lr))) | o < 0 = Moment (SteppingL (-o+1), Maze (Jump (o+1), lr))
                                         | o > 0 = Moment (SteppingR (o+1), Maze (Jump (o+1), lr))
                                         | otherwise = Moment (Ready, Maze (Jump (o+1), lr))
move (Moment (SteppingL 0, z)) = Moment (Ready, z)
move (Moment (SteppingR 0, z)) = Moment (Ready, z)
move (Moment (SteppingL n, z)) = Moment (SteppingL (n-1), moveL z)
move (Moment (SteppingR n, z)) = Moment (SteppingR (n-1), moveR z)
move (Moment (Done, z)) = Moment (Done, z)

strip :: Moment -> (Action, Instr)
strip (Moment (a, Maze (m, _))) = (a, m)

main :: IO ()
main = do
    js <- fmap (map (Jump . readNum) . lines) $ readFile "input.txt"
    let jumps = Moment (Ready, Maze (head js, (repeat Blank, tail js ++ repeat Blank)))
        moments = iterate move jumps
    mapM_ print moments

