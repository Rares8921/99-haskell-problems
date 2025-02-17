{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

main :: IO ()
main = do
    putStrLn ""
    -- 70C.
    print(nnodes tree2)
    putStrLn ""
    -- 70.
    print(stringToTree "afg^^c^bd^e^^^")
    print(treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]))
    putStrLn ""
    -- 71.
    print(ipl tree5)
    print(ipl tree4)
    putStrLn ""
    -- 72.
    print(bottom_up tree5)
    putStrLn ""
    -- 73.
    print(lisp tree1) 
    print(lisp tree2)
    print(lisp tree3)
    print(lisp tree4)
    print(lisp tree5)
    putStrLn ""

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

-- 70C. Counting the nodes of a multiway tree
nnodes :: Tree a -> Int
nnodes (Node _ ts) = 1 + sum (map nnodes ts)

-- 70 Construct a multiway tree from a node string. 
-- "We suppose that the nodes of a multiway tree contain single characters.
--  In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever,
--  during the tree traversal, the move is a backtrack to the previous level."
stringToTree :: String -> Tree Char
stringToTree (c:cs) = Node c (fst (stringToTree' cs))

stringToTree' :: String -> ([Tree Char], String)
stringToTree' ('^':cs) = ([], cs) -- empty node with the remaining string
stringToTree' (c:cs) = (Node c ts : ts', cs'')  -- current node, remaining list and the remaining string
    where (ts, cs') = stringToTree' cs -- children of the current node and the remaining string
          (ts', cs'') = stringToTree' cs' -- children of the remaining list and the remaining string


treeToString :: Tree Char -> String
treeToString (Node c ts) = c : concatMap treeToString ts ++ "^"


-- 71. Determine internal path length of multiway tree. 
-- We define the internal path length of a multiway tree as the total sum of the path lengths
-- from the root to all nodes of the tree.

ipl :: Tree a -> Int
ipl t = ipl' 0 t
    -- Add the current level to the sum of the children
    where ipl' n (Node _ ts) = n + sum (map (ipl' (n + 1)) ts)


-- 72. Construct the bottom-up order sequence of the tree nodes
-- Analogy with the postorder traversal of binary trees, but there it is generalized to get all children
-- from left to right, in this respective order, and then the current root.
bottom_up :: Tree a -> [a]
bottom_up (Node c ts) = concatMap bottom_up ts ++ [c]


-- 73. Lisp-like tree representation
-- "Note that in the "lispy" notation a node with successors (children) in the tree is always the
--  first element in a list, followed by its children. The "lispy" representation of a multiway tree is a
-- sequence of atoms and parentheses '(' and ')', which we shall collectively call "tokens" ".

--  For example, display lisp tree2 = "(a b)"
lisp :: Tree Char -> String
lisp (Node c []) = [c] -- if the node has no children, return the node
-- if the node has children, return the node and the children in a list
lisp (Node c ts) = trim (" (" ++ [c] ++ " " ++ concatMap lisp ts ++ ") ")
    where trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ') -- remove leading and trailing spaces
-- We firstly reverse the string, after remove leading spaces (so the trailing spaces in the original string).
-- Secondly, we reverse again to have the original string, and remove the leading spaces. 