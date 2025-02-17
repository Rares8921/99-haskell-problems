{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Monad (replicateM) -- used in 48
import Data.List (sortBy) -- used in 50

main :: IO ()
main = do
    table (\a b -> (and' a (or' a b)))
    table (\a b -> a `and'` (a `or'` not b))
    tablen 3 and
    tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
    print (gray 3)
    print (huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)])

-- 46. Define the predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- Write a predicate table/3 which prints the truth table of a given logical
--  expression in two variables. 
and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

or' False False = False
or' _     _     = True

not' :: Bool -> Bool
not' True = False
not' False = True

nand' a b = not' (and' a b)

nor'  a b = not' (or'  a b)

xor' True  False = True
xor' False True  = True
xor' _     _     = False

impl' a b = not' a `or'` b

equ' True  True  = True
equ' False False = True
equ' _     _     = False

-- unwords concatenates the strings with spaces between them
table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ (putStrLn . unwords . map show)
            [[a, b, f a b] | a <- [True, False], b <- [True, False]]

--47. Continue Problem 46 by defining and/2, or/2, etc as being infix operators.
-- The higher the number, the higher the precedence

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 7 `impl'`
infixl 7 `equ'`
infixl 8 `not'`

-- 48. Generalize Problem 47 in such a way that the logical expression may contain any
-- number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr,
-- which contains the logical variables enumerated in List. 
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ (putStrLn . formatRow) (args n)
    where
        args n = replicateM n [True, False]  -- Generate combinations of n boolean values
        formatRow a = unwords (map show a) ++ " => " ++ show (f a)  -- Format every row


-- 49. Generate gray codes of length n
gray :: Int -> [String]
gray 0 = [""]
gray n = let
        previousGrayCode = gray (n - 1)
        prefixZero = map ('0':) previousGrayCode
        prefixOne = map ('1':) (reverse previousGrayCode)
    in prefixZero ++ prefixOne

-- 50. Implement huffman encoding

-- Define data types for huffman tree
data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree deriving Show

-- Obtain the frequency of a node
freq :: HuffmanTree -> Int
freq (Leaf _ f) = f
freq (Node f _ _) = f

-- Construct huffman tree from given list
buildHuffmanTree :: [(Char, Int)] -> HuffmanTree
buildHuffmanTree freqs = build (map (uncurry Leaf) freqs)
  where
    build [tree] = tree
    build trees = build (sortBy (\t1 t2 -> compare (freq t1) (freq t2)) $ combine trees)

    -- Merge two nodes
    combine (t1:t2:xs) = Node (freq t1 + freq t2) t1 t2 : xs
    combine xs = xs  -- Pattern matching case

-- After we've built the tree, generate the codes
getHuffmanCodes :: HuffmanTree -> [(Char, String)]
getHuffmanCodes (Leaf c _) = [(c, "")]
getHuffmanCodes (Node _ left right) = 
    -- We are placing 0 on the edge when we are going left and 1 when we are going right
  [(c, '0' : code) | (c, code) <- getHuffmanCodes left] ++
  [(c, '1' : code) | (c, code) <- getHuffmanCodes right]


huffman :: [(Char, Int)] -> [(Char, String)]
huffman = getHuffmanCodes . buildHuffmanTree
