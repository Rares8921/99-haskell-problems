{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

main :: IO ()
main = do
    putStrLn ""
    -- 61.
    print(countLeaves tree4)
    putStrLn ""
    -- 61A.
    print(leaves tree4)
    putStrLn ""
    -- 62.
    print(internals tree4)
    putStrLn ""
    -- 62B.
    print(atLevel tree4 2)
    putStrLn ""
    -- 63.
    print(completeBinaryTree 4)
    putStrLn ""
    -- 64.
    print(layout tree64)
    putStrLn ""
    -- 65.
    print(layout2 tree65)
    putStrLn ""
    -- 66.
    print(layout3 tree65)
    putStrLn ""
    -- 67.
    print(stringToTree "x(y,a(,b))")
    putStrLn ""
    -- 68.
    print(preorder tree4)
    print(inorder tree4)
    putStrLn ""
    -- 69.
    print(ds2tree "xy..z0...")
    print(tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty)))
    putStrLn ""

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- 61. Count the leaves of a binary tree
countLeaves :: Tree a -> Int
countLeaves Empty = 0 -- Base case
countLeaves (Branch _ Empty Empty) = 1 -- Leaf found
countLeaves (Branch _ left right) = countLeaves left + countLeaves right -- Leaves of left and right subtrees


-- 61A. Collect the leaves of a binary tree in a list.
leaves :: Tree a -> [a]
leaves Empty = [] -- Base case
leaves (Branch x Empty Empty) = [x] -- Leaf found
leaves (Branch _ left right) = leaves left ++ leaves right -- Leaves of left and right subtrees


-- 62. Collect the internal nodes of a binary tree in a list
-- An internal node of a binary tree has either one or two non-empty successors
internals :: Tree a -> [a]
internals Empty = [] -- Base case
internals (Branch _ Empty Empty) = [] -- Leaf found
internals (Branch x left right) = x : internals left ++ internals right -- Internal node found


-- 62B. Collect the nodes at a given level in a list
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = [] -- Base case
atLevel (Branch x _ _) 1 = [x] -- Node found at level 1 (we subtract the level by 1 until we reach the last level)
atLevel (Branch _ left right) n = atLevel left (n - 1) ++ atLevel right (n - 1) -- Nodes found at level n


-- 63. Construct a complete binary tree
-- " The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
--   In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted".
--   This means that in a levelorder tree traversal all internal nodes come first, the leaves come second,
--   and empty successors (the nil's which are not really nodes!) come last. "
completeBinaryTree :: Int -> Tree String
completeBinaryTree n = generate 1
    where generate x
            | x > n = Empty -- if we surpass the number of nodes, we return an empty tree
            | otherwise = Branch "nice" (generate (2 * x)) (generate (2 * x + 1)) -- we generate the left and right subtrees


-- 64. Layout algorithm for displaying trees. 
-- Write a function to annotate each node of the tree with a position,
-- where (1,1) in the top left corner or the rectangle bounding the drawn tree. 

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )


layout :: Tree a -> Tree (a, (Int, Int))
layout t = fst $ layoutAux 1 1 t -- Start with initial coordinates (1, 1)
    where
        layoutAux :: Int -> Int -> Tree a -> (Tree (a, (Int, Int)), Int)
        layoutAux x _ Empty = (Empty, x) -- If the tree is empty, return an empty tree and the current x-coordinate
        layoutAux x y (Branch a l r) =
            -- Recursively layout the left subtree, starting with the current x and incrementing y
            let (leftTree, x1) = layoutAux x (y + 1) l
            -- Recursively layout the right subtree, starting with the next x after the left subtree and incrementing y
                (rightTree, x2) = layoutAux (x1 + 1) (y + 1) r
            -- Return the current branch with its coordinates, the left and right subtrees, and the next x-coordinate
            in (Branch (a, (x1, y)) leftTree rightTree, x2)


-- 65. Layout algorithm for displaying trees (part 2). 
-- Find out the rules and write the corresponding function.
-- Hint: On a given level, the horizontal distance between neighboring nodes is constant. 

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

layout2 :: Tree a -> Tree (a, (Int, Int))
layout2 t = layoutAux x1 1 sep1 t
    where 
          d = depth t -- depth of current tree
          ld = leftDepth t -- depth of left subtree
          x1 = 2 ^ (d - 1) - 2 ^ (d - ld) + 1 -- x-coordinate of root node
          sep1 = 2 ^ (d - 2) -- separation between nodes
          
          -- Consider the x coordinate calculated with the separation between nodes to be sep1
          layoutAux :: Int -> Int -> Int -> Tree a -> Tree (a, (Int, Int))
          layoutAux _ _ _ Empty = Empty
          layoutAux x y sep (Branch a l r) = 
                Branch  (a, (x, y)) -- Assign coordinates to the current node
                        (layoutAux (x - sep) (y + 1) (sep `div` 2) l) -- Layout the left subtree
                        (layoutAux (x + sep) (y + 1) (sep `div` 2) r) -- Layout the right subtree
          
          depth :: Tree a -> Int
          depth Empty = 0
          depth (Branch _ l r) = 1 + max (depth l) (depth r)
          
          leftDepth :: Tree a -> Int
          leftDepth Empty = 0
          leftDepth (Branch _ l _) = 1 + leftDepth l


-- 66. Layout algorithm for displaying trees (part 3).
-- " The method yields a very compact layout while maintaining a certain symmetry in every node.
--   Find out the rules and write the corresponding predicate.
--   Hint: Consider the horizontal distance between a node and its successor nodes.
--   How tight can you pack together two subtrees to construct the combined binary tree?"

layout3 :: Tree a -> Tree (a, (Int, Int))
layout3 t = let (leftOffsets, treeWithCoords, _) = layoutHelper initialX 1 t
                initialX = maximum leftOffsets + 1
            in treeWithCoords

layoutHelper :: Int -> Int -> Tree a -> ([Int], Tree (a, (Int, Int)), [Int])
layoutHelper _ _ Empty = ([], Empty, [])
layoutHelper x y (Branch value left right) = 
    let (leftLeftOffsets, leftTree, leftRightOffsets) = layoutHelper (x - separation) (y + 1) left
        (rightLeftOffsets, rightTree, rightRightOffsets) = layoutHelper (x + separation) (y + 1) right
        separation = maximum (0 : zipWith (+) leftRightOffsets rightLeftOffsets) `div` 2 + 1
        newLeftOffsets = 0 : mergeOffsets (map (+ separation) leftLeftOffsets) (map (subtract separation) rightLeftOffsets)
        newRightOffsets = 0 : mergeOffsets (map (+ separation) rightRightOffsets) (map (subtract separation) leftRightOffsets)
    in (newLeftOffsets, Branch (value, (x, y)) leftTree rightTree, newRightOffsets)

mergeOffsets :: [a] -> [a] -> [a]
mergeOffsets [] ys = ys
mergeOffsets xs [] = xs
mergeOffsets (x:xs) (_:ys) = x : mergeOffsets xs ys


-- 67A. A string representation of binary trees
-- Somebody represents binary trees as strings of the following type: a(b(d,e),c(,f(g,)))
stringToTree :: String -> Tree Char
stringToTree "" = Empty
stringToTree [x] = Branch x Empty Empty
stringToTree (x:'(':xs) = Branch x (stringToTree left) (stringToTree right)
    where 
        (left, rightWithParent) = break (== ',') xs  -- Split the string at the first comma to separate left and right subtrees
        right = init (tail rightWithParent)  -- Remove the enclosing parentheses from the right subtree string
stringToTree _ = error "Invalid input"

-- In the problem statement, it says to implement the reverse operation as well.
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"

-- 68. Preorder and inorder sequences of binary trees. 
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Branch x l r) = x : preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Branch x l r) = inorder l ++ [x] ++ inorder r


-- 69. Dotstring representation of binary trees
-- Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where
--      an empty subtree (nil) is encountered during the tree traversal.
-- One example: 
-- tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty)) should return
-- returns "xy..z0..."
ds2tree :: String -> Tree Char
ds2tree = fst . ds2treeAux
    where
        ds2treeAux :: String -> (Tree Char, String)
        ds2treeAux "" = (Empty, "") -- Base case
        ds2treeAux (x:xs) = if x == '.' -- if nil node found, add empty node and continue with the rest of the string
                                then (Empty, xs)
                            else 
                                let (left, rest1) = ds2treeAux xs -- recursively build left subtree
                                    (right, rest2) = ds2treeAux rest1 -- recursively build right subtree
                                in (Branch x left right, rest2)  -- return the tree and the remaining string

-- Now the reverse operation
tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x : tree2ds l ++ tree2ds r