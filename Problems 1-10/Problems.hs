module Main where

import Data.List

main :: IO ()
main = do
    --print(problem1 [1,2,3,4])
    --print(problem2 [1,2,3,4])
    --print(problem3 [1,2,3] 2)
    --print(problem4 [123, 456, 789])
    --print(problem5 "A man, a plan, a canal, panama!")
    --print(problem6 "racecar")
    --print(problem7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    --print(problem8 "aaaabccaadeeee")
    --print(problem9 "aaaabccaadeeee")
    print(problem10 "aaaabccaadeeee")


-- (*) Find the last element of a lists
problem1 :: [a] -> a
problem1 [x] = x
problem1 (_:xs) = problem1 xs


-- (*) Find the last-but-one (or second-last) element of a list
problem2 :: [a] -> a
problem2 [x, _] = x
problem2 (_:xs) = problem2 xs


-- (*) Find the K'th element of a list. 
problem3 :: [a] -> Int -> a
problem3 (x:xs) k = if length (x:xs) == k then x else problem3 xs k 

-- or problem3 [x] k = x !! (k - 1)


-- (*) Find the number of elements in a list.
problem4 :: [a] -> Integer
problem4 [] = 0
problem4 [x] = 1
problem4 (x:xs) = 1 + problem4 xs


-- (*) Reverse a list.
problem5 :: [a] -> [a]
problem5 [] = []
problem5 [x] = [x]
problem5 (x:xs) = problem5 xs ++ [x]


-- (*) Find out whether a list is a palindrome. 
problem6 :: Eq a => [a] -> Bool
problem6 xs = xs == problem5 xs


-- (**) Flatten a nested list structure.
data NestedList nl = Elem nl | List [NestedList nl] -- this code is from the website
problem7 :: NestedList a -> [a]
problem7 (Elem x) = [x]
problem7 (List xs) = flatten xs

-- Helper function
flatten :: [NestedList a] -> [a]
flatten [] = []
flatten (x:xs) = problem7 x ++ flatten(xs)

-- (**) Eliminate consecutive duplicates of list elements. 
problem8 :: Eq a => [a] -> [a]
problem8 [] = []
problem8 [x] = [x]
problem8 [x,y] = if x == y then [x] else [x, y]
problem8 (x:y:xs) = if x == y then problem8 (y:xs) else x : problem8(y:xs)


-- (**) Pack consecutive duplicates of list elements into sublists.
problem9 :: Eq a => [a] -> [[a]]
problem9 [] = []
problem9 (x:xs) = pack [x] xs

-- Helper function
pack :: Eq a => [a] -> [a] -> [[a]]
pack x [] = [x]
pack current@(x:_) (y:ys) = if x == y then pack (y:current) ys else current : pack [y] ys

-- (*) Run-length encoding of a list. Solutions 
problem10 :: Eq a => [a] -> [(Int, a)]
problem10 [] = []
problem10 x = encoder (problem9 x)

-- Helper function
encoder :: [[a]] -> [(Int, a)]
encoder [] = []
encoder (x:xs) = (length x, x!!0) : encoder xs
