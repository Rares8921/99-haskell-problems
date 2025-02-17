module Main where

import Data.List

main :: IO ()
main = do
    --print(problem11 "aaaabccaadeeee")
    --print(problem12 (problem11 "aaaabccaadeeee"))
    --print(problem14 [1, 2, 3])
    --print(problem15 [1, 2, 3] 3)
    --print(problem16 "abcdefghik" 3)
    --print(problem17 "abcdefghik" 3)
    --print(problem18 ['a','b','c','d','e','f','g','h','i','k'] 3 7)
    --print(problem19 ['a','b','c','d','e','f','g','h'] (-2))
    print(problem20 2 "abcd")


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

-- (*) Modified run-length encoding. 
problem11 :: (Eq a, Show a) => [a] -> [String]
problem11 x = encodeModified (problem10 x)

encodeModified :: (Eq a, Show a) => [(Int, a)] -> [String]
encodeModified [] = []
encodeModified ((cnt, x):xs) = if cnt == 1 then ("Single " ++ show x) : encodeModified xs else ("Multiple " ++ show cnt ++ " " ++ show x) : encodeModified xs

-- (**) Decode a run-length encoded list. 
-- http://www.zvon.org/other/haskell/Outputprelude/replicate_f.html
problem12 :: [String] -> String
problem12 [] = []
problem12 (x:xs) = case words x of
        ("Single " : val : _) -> val ++ problem12 xs
        ("Multiple " : cnt : val : _) -> replicate (read cnt :: Int) (head val) ++ problem12 xs
        _ -> "Invalid"


-- Problem 13 is the same as problem 11


-- (*) Duplicate the elements of a list. Solutions 
problem14 :: [a] -> [a]
problem14 [] = []
problem14 (x:xs) = x : x : problem14 xs


-- (**) Replicate the elements of a list a given number of times. 
problem15 :: [a] -> Int -> [a]
problem15 [] _ = []
problem15 (x:xs) cnt = replicate cnt x ++ problem15 xs cnt


-- (**) Drop every N'th element from a list.
problem16 :: [a] -> Int -> [a]
problem16 x y = dropElem x y 1

dropElem :: [a] -> Int -> Int -> [a]
dropElem [] _ _ = []
dropElem (x:xs) k cnt = if cnt `mod` k == 0 then dropElem xs k (cnt + 1) else x : dropElem xs k (cnt + 1)


-- (*) Split a list into two parts; the length of the first part is given. Solutions 
problem17 :: Eq a => [a] -> Int -> ([a], [a])
problem17 x y = (retrieve x 0 0 y, retrieve x y 0 (length x))

retrieve :: Eq a => [a] -> Int -> Int -> Int -> [a]
retrieve [] _ _ _ = []
retrieve (x:xs) st k y = if k < st then retrieve xs st (k + 1) y else if k < y then x : retrieve xs st (k + 1) y else []


-- (**) Extract a slice from a list. 
problem18 :: Eq a => [a] -> Int -> Int -> [a]
problem18 x y z = retrieve x y 0 (z + 1)


-- (**) Rotate a list N places to the left. Solutions 
problem19 :: Eq a => [a] -> Int -> [a]
problem19 x n = if n == 0 then x else if n > 0 then retrieve x n 0 (length x) ++ retrieve x 0 0 n else retrieve x (length x + n) 0 (length x) ++ retrieve x 0 0 (length x + n)

-- (*) Remove the K'th element from a list 
problem20 :: Eq a => Int -> [a] -> ([a], [a])
problem20 k x = (retrieve x (k - 1) 0 (k), retrieve x 0 0 (k - 1) ++ retrieve x k 0 (length x)) 
