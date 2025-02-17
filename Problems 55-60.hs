{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

main :: IO ()
main = do
    putStrLn ""
    -- 55
    putStrLn $ concatMap (\t -> show t ++ "\n") (generateBalancedTrees "nice" 4)
    -- 56
    print(symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty))
    print(symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)))
    putStrLn ""
    -- 57
    print(construct [3, 2, 5, 7, 1])
    print(symmetric . construct $ [5, 3, 18, 1, 4, 12, 21])
    print(symmetric . construct $ [3, 2, 5, 7, 1])
    putStrLn ""
    -- 58
    print(symCbalTrees "nice" 5)
    putStrLn ""
    -- 59.
    --print(hbaltree "nice" 5) -- output is too large
    print(take 4 $ hbaltree 'x' 3)
    putStrLn ""
    -- 60.
    print(length $ hbalTreeNodes 'x' 15)
    print(map (hbalTreeNodes 'x') [0..3])
    putStrLn ""

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- 55. Construct completely balanced binary trees. 
-- Firstly, we want to check the condition for balance:
-- abs(number of nodes in left subtree - number of nodes in right subtree) <= 1 

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

isBalancedTree :: Tree a -> Bool
isBalancedTree Empty = True
isBalancedTree (Branch _ l r) = abs (countNodes l - countNodes r) <= 1
                                && isBalancedTree l && isBalancedTree r

generateTrees :: a -> Int -> [Tree a]
generateTrees _ 0 = [] -- In case of empty input
generateTrees c 1 = [Branch c Empty Empty] -- Add the last node
generateTrees c n = [Branch c t Empty | t <- subtree] ++ -- Add subtree to either left or right side
                    [Branch c Empty t | t <- subtree] ++
                    -- Splitting the remaining nodes into left and right subtrees:
                    concat [[Branch c l r | l <- fst lrtrees, r <- snd lrtrees] | lrtrees <- treeMinusTwo]
    where
        subtree = generateTrees c (n - 1)
        -- treeMinusTwo generates pairs (leftSubtrees, rightSubtrees), used in combining every possible
        -- left tree with every possible right tree. the sizes of the subtree will be num and n - num - 1
        -- as for every number generated, we want to have in the other side the correct number of nodes
        treeMinusTwo = [(generateTrees c num, generateTrees c (n - num - 1)) | num <- [0..n-2]]

generateBalancedTrees :: a -> Int -> [Tree a]
generateBalancedTrees c n = filter isBalancedTree (generateTrees c n)


-- 56. Generate symetric binary trees.
-- "Let us call a binary tree symmetric if you can draw a vertical line through the root node and then 
--  the right subtree is the mirror image of the left subtree." 
-- Write a predicate symmetric/1 to check whether a given binary tree is symmetric

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r


-- 57. Construct binary search tree from list
-- Firstly, we have an auxiliary function to add a node inside the tree
-- If the element already exists, the tree remains unchanged.
add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty -- If the (sub)tree is empty, create a new root.
add x (Branch y l r) = if x < y then Branch y (add x l) r -- If node < root then go left
    else if x > y then Branch y l (add x r) -- Otherwise, go right
    else Branch y l r

-- The elements are inserted one by one using the `add` function.
construct :: Ord a => [a] -> Tree a
-- We need flip because add expects the value first and then the tree
-- We use foldl because we need to process the elements in order, from left to right
-- and to not make the tree right-heavy
construct xs = foldl (flip add) Empty xs -- Inserts each element into the tree starting from Empty.


-- 58. Apply the generate-and-test paradigm to construct all symmetric,
-- completely balanced binary trees with a given number of nodes. 
symCbalTrees :: a -> Int -> [Tree a]
symCbalTrees x = filter symmetric . generateBalancedTrees x


-- 59. Construct height-balanced binary trees. 
-- "In a height-balanced binary tree, the following property holds for every node: 
--  The height of its left subtree and the height of its right subtree are almost equal, 
--  which means their difference is not greater than one. "
hbaltree :: a -> Int -> [Tree a]
hbaltree x 0 = [Empty]
hbaltree x 1 = [Branch x Empty Empty] -- Root of (sub)tree
hbaltree x h = [Branch x l r |
        -- To mantain the height balance, there are only 3 posibilties
        -- 1) We make the left subtree to be smaller
        -- 2) We preserve the same height in each subtree
        -- 3) We make the right subtree to be smaller
        (hl, hr) <- [(h - 2, h - 1), (h - 1, h - 1), (h - 1, h - 2)],
        l <- hbaltree x hl, r <- hbaltree x hr]


-- 60. Construct height-balanced binary trees with a given number of nodes. 
-- Fibonacci numbers

-- The list 'fibo' generates Fibonacci numbers starting with 0 and 1.
fibo :: [Int]
fibo = 0 : 1 : zipWith (+) fibo (tail fibo)

-- Calculate the minimum number of nodes in a weight-balanced tree of height 'h'.
-- The minimum number of nodes follows the Fibonacci sequence.
minNodes :: Int -> Int
minNodes h = fibo !! (h + 2) - 1  -- Use Fibonacci sequence to determine the minimum nodes

-- Calculate the maximum number of nodes in a weight-balanced tree of height 'h'.
-- A perfect binary tree has 2^h - 1 nodes at height 'h'.
maxNodes :: Int -> Int
maxNodes h = 2 ^ h - 1  -- A full binary tree at height 'h' has this many nodes.

-- Given a number of nodes 'n', find the minimum height of a weight-balanced tree.
-- The height is calculated using log base 2 of (n+1).
minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n + 1)  -- log2(n+1) to find the minimum height

-- Given a number of nodes 'n', find the maximum height of a weight-balanced tree.
-- This calculates the largest height possible for a tree with 'n' nodes.
maxHeight :: Int -> Int
maxHeight n = length (takeWhile (<= n + 1) fibo) - 3
-- Fibonacci numbers give the height for trees with 'n' nodes.
-- Subtract 3 because of how Fibonacci indexing works.

-- Generate all possible height-balanced binary trees with 'n' nodes, 
-- where each tree's nodes are labeled with the value 'x'.
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = [t | h <- [minHeight n .. maxHeight n], t <- balancedtree h n]
  where
        -- 'balancedtree h n' generates weight-balanced trees of height 'h' with 'n' nodes
        -- We calculate trees for heights from 'minHeight n' to 'maxHeight n'
        
        balancedtree 0 n = [Empty]
        balancedtree 1 n = [Branch x Empty Empty]
        balancedtree h n = 
            [Branch x l r |
                -- The same logic as earlier in 59
                (hl, hr) <- [(h - 2, h - 1), (h - 1, h - 1), (h - 1, h - 2)],
                
                -- Calculate the range for the number of nodes in the left subtree
                let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
                let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
                
                -- Iterate over possible sizes for the left subtree
                nl <- [min_nl .. max_nl],
                
                -- The remaining nodes go to the right subtree
                let nr = n - 1 - nl,
                
                -- Recursively generate all balanced trees for the left and right subtrees
                l <- balancedtree hl nl,
                r <- balancedtree hr nr
            ]

