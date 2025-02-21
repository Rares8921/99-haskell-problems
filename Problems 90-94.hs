{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.List -- 91, 92
import Data.Ord (comparing) -- 91.
import Control.Monad (guard) -- 92, 93

main :: IO ()
main = do
    putStrLn ""
    -- 90.
    print(length (queens 8))
    print(head (queens 8))
    putStrLn ""
    -- 91.
    print(head $ knightsTo 8 (1,1))
    print(head $ closedKnights 8)
    putStrLn ""
    -- 92. 
    print(head $ vonKoch [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)])
    putStrLn ""
    -- 93.
    mapM_ putStrLn $ puzzle [2,3,5,7,11]
    putStrLn ""
    -- 94. 
    print(length $ regular 6 3)
    putStrLn " "


-- 90. Eight queens problem
-- This is a classical problem in computer science.
-- The objective is to place eight queens on a chessboard so that no two queens are attacking each other;
-- i.e., no two queens are in the same row, the same column, or on the same diagonal.

-- Hint: Represent the positions of the queens as a list of numbers 1..N.
-- Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4,
-- the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

isSafe :: Int -> [Int] -> Int -> Bool
isSafe _ [] _ = True
-- x is the current queen, y is the queen in the list, n is the distance between the two queens
-- check if the queens are in the same row, column or diagonal
isSafe x (y:ys) n = x /= y && abs (x - y) /= n && isSafe x ys (n + 1) 

queens :: Int -> [[Int]]
queens n = solve n []
  where
    -- placed is the list of queens placed so far
    solve 0 placed = [placed] -- if all queens are placed, return the list of queens
    solve k placed = -- if there are still queens to place
        concat [solve (k - 1) (q : placed) | q <- [1..n], isSafe q placed 1] -- place the queen in the column k


-- 91. Knight's tour
-- Another famous problem is this one: How can a knight jump on an N×N chessboard in such a way that
-- it visits every square exactly once?
-- Hints: Represent the squares by pairs of their coordinates of the form X/Y,
-- where both X and Y are integers between 1 and N.

-- There are two variants of this problem: 
-- 1. find a tour ending at a particular square
-- 2. find a circular tour, ending a knight's jump from the start
--   (clearly it doesn't matter where you start, so choose (1,1))

knightsTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightsTo n (x, y) = solve n [(x, y)]
  where
    -- placed is the list of squares visited so far
    solve 0 placed = [placed] -- if all squares are visited, return the list of squares
    solve k placed = -- if there are still squares to visit
        concat [solve (k - 1) (q : placed) | q <- next (head placed), q `notElem` placed] -- visit the square in the next move

    next (x, y) = [(x + dx, y + dy) | dx <- [2, 2, 1, 1, -1, -1, -2, -2], dy <- [1, -1, 2, -2, 2, -2, 1, -1], abs dx + abs dy == 3, x + dx >= 1, x + dx <= n, y + dy >= 1, y + dy <= n]

-- Check if given coordinates are inside the board
insideBoard :: Int -> (Int, Int) -> Bool
insideBoard size (r, c) = r > 0 && r <= size && c > 0 && c <= size

-- Directions in which a knight can move, stored as a list of pairs of coordinates
knightMoves :: Int -> (Int, Int) -> [(Int, Int)]
knightMoves size (r, c) = 
    [ (r + dr, c + dc) | (dr, dc) <- [(2,1), (2,-1), (1,2), (1,-2), (-1,2), (-1,-2), (-2,1), (-2,-1)]
                        , insideBoard size (r + dr, c + dc) ]

-- Prioritize moves based on the number of moves available from the current position
prioritizeMoves :: Ord b => ((Int, Int) -> b) -> [(Int, Int)] -> [(Int, Int)]
prioritizeMoves f list = map snd (sortBy (comparing fst) [(f x, x) | x <- list])

-- Find a circular tour, ending a knight's jump from the start
closedKnights :: Int -> [[(Int, Int)]]
closedKnights size = [p:path | (p, path) <- constructPaths (size*size), p == (1,1)]
  where
    -- Construct paths of length k
    constructPaths 1 = [((2,3), [])] -- ending at (2,3) is the only possible move
    -- For each path of length k-1, find the next move and construct paths of length k
    constructPaths k = [(next, p:path) |
                        (p, path) <- constructPaths (k-1),
                        next <- prioritizeMoves (countMoves path)
                                 (filter (`notElem` path) (knightMoves size p))]
    -- Count the number of moves available from the current position
    countMoves path pos
      | pos == (1,1) = maxBound 
      | otherwise = length (filter (`notElem` path) (knightMoves size pos))


-- 92. Von Koch's conjecture
-- Given a tree with N nodes (and hence N-1 edges). Find a way to enumerate the nodes from 1 to N and, 
-- accordingly, the edges from 1 to N-1 in such a way, that for each edge K the difference of its node numbers
-- equals to K. The conjecture is that this is always possible. 

vonKoch :: [(Int, Int)] -> [[(Int, Int)]]
vonKoch edges = generate (reverse adjacency) [] [] []
  where
    -- Base case: no edges left to process, return the result
    generate [] _ _ result = return result
    
    -- Recursive case: process a vertex and its neighbors
    generate ((v, neighbors):rest) usedVertices usedVerticesV visited =
      do (n, usedVertices') <- [ (x, usedVertices')
                               | x <- [1..limitV],
                                 x `notElem` usedVertices,  -- Ensure vertex is not visited before
                                 let edgesToCheck = map (abs . (x -)) (map (findN visited) neighbors),  -- Create the edges
                                 let (usedVertices', valid) = foldl (\(l, f) d ->
                                         (d:l, f && d `notElem` l && 1 <= d && d <= limitM))  -- Check validity of edges
                                         (usedVertices, True) edgesToCheck, valid]
         -- Recur on the remaining vertices
         generate rest usedVertices' (n:usedVerticesV) ((v, n):visited)

    limitM = length edges  -- Maximum number of edges
    limitV = limitM + 1  -- Maximum number of vertices
    
    -- Degree of a vertex: number of edges connected to it
    degree x = length $ filter (\(a, b) -> a == x || b == x) edges
    
    -- Generate adjacency list: for each vertex, list its neighbors
    createAdjacency [] = []
    createAdjacency (v:vs) = (v, [u | u <- vs, (v, u) `elem` edges || (u, v) `elem` edges]):createAdjacency vs
    
    -- Find the vertex 'n' in the visited list
    findN visited u = snd $ head $ filter ((== u) . fst) visited
    
    adjacency = createAdjacency $ sortBy (comparing degree) [1..limitV]  -- Create adjacency list based on vertex degree


-- 93. An arithmetic puzzle
-- Given a list of integer numbers, find a correct way of inserting arithmetic signs (operators) such that
-- the result is a correct equation.
-- Example: With the list of numbers [2,3,5,7,11] we can form the equations 2-3+5+7 = 11 or 2 = 3-5+7+11.

puzzle :: [Integer] -> [String]
puzzle numbers = concatMap (findEquations numbers) [1..length numbers - 1]
  where
    -- For each split point `i`, find possible equations
    findEquations :: [Integer] -> Int -> [String]
    findEquations nums i = do
      let (leftPart, rightPart) = splitAt i nums
      (lhsExpr, lhsVal) <- generateExpression leftPart
      (rhsExpr, rhsVal) <- generateExpression rightPart
      guard (lhsVal == rhsVal)  -- Only keep valid equations
      return (lhsExpr ++ " = " ++ rhsExpr)

-- Generate expressions from a list of integers
generateExpression :: [Integer] -> [(String, Rational)]
generateExpression [x] = return (show x, fromInteger x)

generateExpression nums = do
  i <- [1..length nums - 1]
  let (leftPart, rightPart) = splitAt i nums
  (leftExpr, leftVal) <- generateExpression leftPart
  (rightExpr, rightVal) <- generateExpression rightPart
  (operator, opFunc) <- [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]
  
  -- Skip invalid operations (e.g., division by zero)
  guard (validOperation operator rightVal leftVal)
  
  -- Combine the subexpressions with the operator
  let left = if operator `elem` ["*", "/"] && leftExpr /= "_" 
             then "(" ++ leftExpr ++ ")"
             else leftExpr
  let right = if operator `elem` ["-", "*", "/"] && rightExpr /= "_"
              then "(" ++ rightExpr ++ ")"
              else rightExpr
  return (left ++ " " ++ operator ++ " " ++ right, opFunc leftVal rightVal)

-- Check if the operation is valid (e.g., no division by zero)
validOperation :: String -> Rational -> Rational -> Bool
validOperation operator rhsVal lhsVal
  | operator == "/" && rhsVal == 0 = False
  | operator == "+" && (lhsVal == 0 || rhsVal == 0) = False
  | otherwise = True

-- 94. Generate K-regular simple graphs with N nodes. 
-- In a K-regular graph, each node has exactly K neighbors.
-- For example, we find how many (non-isomorphic!) 3-regular graphs with 6 nodes exist.

data Graph a = Graph [a] [(a, a)]
  deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
  deriving (Show, Eq)

-- Generate all combinations (of k distinct elements)
comb :: [a] -> Int -> [[a]]
comb _ 0      = [[]]
comb [] _     = []
comb (x:xs) k = map (x:) (comb xs (k-1)) ++ comb xs k

-- Convert a Graph to its Adjacency representation.

convertGraphToAdj :: Eq a => Graph a -> Adjacency a
convertGraphToAdj (Graph vs es) =
  Adj [ (v, [ if v == a then b else a | (a,b) <- es, v == a || v == b ])
      | v <- vs
      ]

-- Convert an Adjacency to a Graph (avoiding duplicate edges).
convertAdjToGraph :: Eq a => Adjacency a -> Graph a
convertAdjToGraph (Adj xs) =
  Graph (map fst xs) (foldl collect [] xs)
  where
    collect acc (v, nbrs) =
      acc ++ [ (v, n) | n <- nbrs, not (edgeExists (v, n) acc) ]
    edgeExists (x,y) es = (x,y) `elem` es || (y,x) `elem` es


-- Generates a mapping from vertices to numbers using all permutations of [1..n] and then picks 
-- the lexicographically minimum encoding.
canonicalForm :: (Eq a, Ord a) => Adjacency a -> String
canonicalForm (Adj xs) = minimum [ encodeMapping sigma | sigma <- permutations [1..n] ]
  where
    n        = length xs
    verts    = map fst xs
    encodeMapping sigma =
      let mapping  = zip verts sigma
          mapValue v = case lookup v mapping of
                         Just num -> num
                         Nothing  -> error "Mapping error"
          encode (v, ns) = ( mapValue v, sort (map mapValue ns) )
          encoded  = map encode xs
      in show (sortOn fst encoded)


-- Remove duplicate elements based on a key function.
uniqueBy :: Eq b => (a -> b) -> [a] -> [a]
uniqueBy _ []     = []
uniqueBy key (x:xs) = x : uniqueBy key (filter (\y -> key y /= key x) xs)

-- Generate all K-regular graphs with N nodes.
regular :: Int -> Int -> [Graph Int]
regular n k
  | r == 1 || n <= k || n < 0 || k < 0 = []
  | otherwise =
      map (convertAdjToGraph . fst) uniqueAdjacencies
  where
    (q, r) = (n * k) `quotRem` 2
    -- All possible edges (only one copy per unordered pair)
    allEdges = [ (i, j) | i <- [1..n], j <- [i+1..n] ]
    -- Every candidate is a selection of q edges from allEdges.
    candidateEdges = comb allEdges q
    -- Build graphs with vertex set [1..n] and a candidate set of edges.
    candidateGraphs = [ Graph [1..n] es | es <- candidateEdges ]
    -- Convert each graph into an adjacency list.
    candidateAdj = map convertGraphToAdj candidateGraphs
    -- Keep only those graphs where every vertex has exactly k neighbors.
    validAdj = filter (\(Adj lst) -> all ((== k) . length . snd) lst) candidateAdj
    -- Tag each valid adjacency with its canonical representation.
    tagged   = [ (adj, canonicalForm adj) | adj <- validAdj ]
    -- Remove duplicates (keeping one representative per canonical form).
    uniqueAdjacencies = uniqueBy (\(_, canon) -> canon) tagged
  