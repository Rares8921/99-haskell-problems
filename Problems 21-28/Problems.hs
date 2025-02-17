module Main where

import System.Random -- problem 23
import Data.List -- problem 25

main :: IO ()
main = do
    {--print(problem21 'X' "abcd" 2)
    print(problem22 4 9)
    result <- problem23 "abcdefgh" 3
    print result
    result <- problem24 6 49
    print(result) 
    result <- problem25 "abcdef"
    print(result) 
    print(problem26 3 "abcdef") 
    print(problem27 [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"])--}
    print(problem28 ["abc","de","fgh","de","ijkl","mn","o"])


-- Insert an element at a given position into a list. 
problem21 :: a -> [a] -> Int -> [a];
problem21 _ l 0 = l
problem21 x (h:t) 1 = x:h:t
problem21 x (h:t) p = h : problem21 x t (p - 1)

-- Create a list containing all integers within a given range. 
problem22 :: Int -> Int -> [Int]
problem22 x y = [x..y]

-- Extract a given number of randomly selected elements from a list. 
problem23 :: [a] -> Int -> IO [a]
problem23 x p = mapM (const randomPick) [1..p]
  where
    n = length x
    randomPick = do
        idx <- randomRIO (0, n - 1)
        return (x !! idx)


-- Lotto: Draw N different random numbers from the set 1..M. 
problem24 :: Int -> Int -> IO [Int]
problem24 p n = problem23 [1..n] p

-- Generate a random permutation of the elements of a list.
element :: [a] -> IO a
element xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

problem25 :: [a] -> IO [a]
problem25 x = element . permutations $ x


-- **) Generate combinations of K distinct objects chosen from the N elements of a list. 
-- Group the elements of a set into disjoint subsets. 
-- Sorting a list of lists according to length of sublists. 
problem26 :: Int -> [a] -> [[a]]
problem26 0 _ = [[]]
problem26 n xs = [y:ys | y:zs <- tails xs, ys <- problem26(n - 1) zs]

-- Group the elements of a set into disjoint subsets. 
problem27 :: [Int] -> [a] -> [[[a]]]
problem27 [] _ = [[]]
problem27 (y:ys) xs = [z:zs | (z, gs) <- combination y xs, zs <- problem27 ys gs]

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs = [([],xs)]
combination n [] = []
combination n (x:xs) = [(x:ys,zs) | (ys,zs) <- combination (n-1) xs] ++ [(ys,x:zs) | (ys,zs) <- combination n xs]


-- Sorting a list of lists according to length of sublists. 
problem28 :: [[a]] -> [[a]]
problem28 = sortBy (\x y -> compare (length x) (length y))
