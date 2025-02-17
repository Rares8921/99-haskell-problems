module Main where

import Data.List

main :: IO ()
main = do
    --print(problem31 7)
    --print(problem32 36 63)
    --print(problem33 35 64)
    --print(problem34 10)
    --print(problem35 315)
    --print(problem36 315)
    --print(problem37 10)
    --print(problem39 10 20)
    --print(problem40 28)
    print(problem41 9 20)

-- (**) Determine whether a given integer number is prime.
problem31 :: Int -> Bool
problem31 x = if x < 2 then False else length(divi x) == 2

divi :: Int -> [Int]
divi x = [i | i <- [1..x], x `mod` i == 0]

-- (**) Determine the greatest common divisor of two positive integer numbers. 
problem32 :: Int -> Int -> Int
problem32 a 0 = a
problem32 a b = problem32 b (a `mod` b)

-- (*) Determine whether two positive integer numbers are coprime. 
problem33 :: Int -> Int -> Bool
problem33 a b = problem32 a b == 1

-- (**) Calculate Euler's totient function phi(m). 
problem34 :: Int -> Int
problem34 x = length [i | i <- [1..x], problem33 i x]

-- (**) Determine the prime factors of a given positive integer.
problem35 :: Int -> [Int]
problem35 x = primes x 2

primes :: Int -> Int -> [Int]
primes x d | x < 2 = []
           | d * d > x = [x]
           | x `mod` d == 0 = d : primes (x `div` d) d
           | otherwise = primes x (d + 1)

--(**) Determine the prime factors and their multiplicities of a given positive integer. 
problem36 :: Int -> [(Int, Int)]
problem36 x = encoder(problem9 (primes x 2))

problem9 :: [Int] -> [[Int]]
problem9 [] = []
problem9 (x:xs) = pack [x] xs

-- Helper function
pack :: [Int] -> [Int] -> [[Int]]
pack x [] = [x]
pack current@(x:_) (y:ys) = if x == y then pack (y:current) ys else current : pack [y] ys

-- Helper function
encoder :: [[Int]] -> [(Int, Int)]
encoder [] = []
encoder (x:xs) = (x!!0, length x) : encoder xs

-- (**) Calculate Euler's totient function phi(m) (improved). 
problem37 :: Int -> Int
problem37 x = helper37 (problem36 x)

helper37 :: [(Int, Int)] -> Int
helper37 [] = 1
helper37 ((p1, m1):x) = (p1 - 1) * p1^(m1 - 1) * helper37 x

-- Problem 38 is: (*) Compare the two methods of calculating Euler's totient function. 

-- (*) A list of prime numbers in a given range. 
problem39 :: Int -> Int -> [Int]
problem39 x y = [i | i <- [x..y], problem31 i]

-- (**) Goldbach's conjecture. 
problem40 :: Int -> (Int, Int)
problem40 x = if x <= 2 then (0, 0) else helper40 x (x - 1)

helper40 :: Int -> Int -> (Int, Int)
helper40 _ 0 = (0, 0)
helper40 x n = if problem31 n && problem31 (x - n) then (x - n, n) else helper40 x (n - 1)

-- (**) A list of even numbers and their Goldbach compositions in a given range. 
problem41 :: Int -> Int -> [(Int, Int)]
problem41 a b = [problem40 i | i <- [a..b], i `mod` 2 == 0]
