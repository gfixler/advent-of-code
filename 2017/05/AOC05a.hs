module AOC05a where

import Control.Arrow (first, second)
import qualified Data.List.Zipper as Z


numbers :: String -> IO [Int]
numbers = fmap (map read . lines) . readFile

move :: (Num a, Ord a) => Z.Zipper a -> (Z.Zipper a -> Z.Zipper a)
move z | c < 0 = Z.left
       | c > 0 = Z.right
       | otherwise = id
    where c = Z.cursor z

takeSteps :: Z.Zipper Int -> Z.Zipper Int
takeSteps z = z'' !! (abs c)
    where c = Z.cursor z
          z' = Z.replace (c+1) z
          z'' = iterate (move z) z'

main :: IO ()
main = do
    insts <- numbers "input.txt"
    let l = length insts
        z = Z.fromList insts
    let r = iterate takeSteps z
    print (r !! 325926) -- hacking away

-- Z.beginp :: Z.Zipper a -> Bool
-- Z.cursor :: Z.Zipper a -> a
-- Z.delete :: Z.Zipper a -> Z.Zipper a
-- Z.duplicatez :: Z.Zipper a -> Z.Zipper (Z.Zipper a)
-- Z.empty :: Z.Zipper a
-- Z.emptyp :: Z.Zipper a -> Bool
-- Z.end :: Z.Zipper a -> Z.Zipper a
-- Z.endp :: Z.Zipper a -> Bool
-- Z.extendz :: (Z.Zipper a -> b) -> Z.Zipper a -> Z.Zipper b
-- Z.extractz :: Z.Zipper a -> a
-- Z.foldlz :: (b -> Z.Zipper a -> b) -> b -> Z.Zipper a -> b
-- Z.foldlz' :: (b -> Z.Zipper a -> b) -> b -> Z.Zipper a -> b
-- Z.foldrz :: (Z.Zipper a -> b -> b) -> b -> Z.Zipper a -> b
-- Z.fromList :: [a] -> Z.Zipper a
-- Z.fromListEnd :: [a] -> Z.Zipper a
-- Z.insert :: a -> Z.Zipper a -> Z.Zipper a
-- Z.left :: Z.Zipper a -> Z.Zipper a
-- Z.pop :: Z.Zipper a -> Z.Zipper a
-- Z.push :: a -> Z.Zipper a -> Z.Zipper a
-- Z.replace :: a -> Z.Zipper a -> Z.Zipper a
-- Z.reversez :: Z.Zipper a -> Z.Zipper a
-- Z.right :: Z.Zipper a -> Z.Zipper a
-- Z.safeCursor :: Z.Zipper a -> Maybe a
-- Z.start :: Z.Zipper a -> Z.Zipper a
-- Z.toList :: Z.Zipper a -> [a]
