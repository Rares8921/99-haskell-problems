{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.List (partition, subsequences, nub, permutations, sort, minimumBy, sortBy) -- 82, 83, 85, 86
import Data.Maybe (fromJust) -- 84.

main :: IO ()
main = do
    putStrLn ""
    -- 80.
    print(graphToAdj (Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]))
    putStrLn ""
    -- 81.
    print(paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
    print(paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
    putStrLn ""
    -- 82.
    print(cycles 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
    print(cycles 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
    putStrLn ""
    -- 83.
    print(length $ spanningTree k4)
    putStrLn ""
    -- 84.
    print(prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)])
    putStrLn ""
    -- 85.
    print(iso graphG1 graphH1)
    putStrLn ""
    -- 86.
    print(kColor ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')])
    putStrLn ""
    -- 87.
    print(depthFirst ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1)
    putStrLn ""
    -- 88.
    print(connectedComponents ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]))
    putStrLn ""
    -- 89.
    print(bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]))
    print(bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)]))
    putStrLn ""


data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)
			  
data Adjacency a = Adj [(a, [a])]
		   deriving (Show, Eq)

-- Complete graph K4 definition
k4 :: Graph Int
k4 = Graph [1, 2, 3, 4] [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]



-- 80. Write predicates to convert between the different graph representations.

-- Get a node and the list of edges and return the list of adjacent nodes to current node
getAdj :: (Eq a) => a -> [(a, a)] -> [a]
getAdj _ [] = []
getAdj x ((a, b):ys)
    | x == a = b : getAdj x ys
    | x == b = a : getAdj x ys
    | otherwise = getAdj x ys

-- Get the list of nodes and construct recursively the neighbors of each node
getAdjList :: (Eq a) => [a] -> [(a, a)] -> [(a, [a])]
getAdjList [] _ = []
getAdjList (x:xs) ys = (x, (getAdj x ys)) : getAdjList xs ys

-- Convert a graph to an adjacency list
graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _) = Adj [] -- Empty graph
graphToAdj (Graph (x:xs) ys) = Adj ((x, (getAdj x ys)) : zs)
    where
        zs = getAdjList xs ys


-- 81. Acyclic Paths between two given nodes (in a directed graph).
paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths a b xs = paths' a b xs []

-- All acyclic unique paths between two nodes
paths' :: (Eq a) => a -> a -> [(a, a)] -> [a] -> [[a]]
paths' a b xs ys
    | a == b = [ys ++ [b]] -- Destination reached
    -- Explore nodes and add them to the path
    | otherwise = concat [paths' d b xs (ys ++ [c]) | (c, d) <- xs, c == a, notElem c ys]

{-
    However, for undirected graphs, the code is:
paths' :: (Eq a) => a -> a -> [(a, a)] -> [a] -> [[a]]
paths' a b xs ys
    | a == b = [ys ++ [b]]
    | otherwise = concat [paths' d b (filter (\(x, y) -> x /= c && y /= c) xs) (ys ++ [c]) | (c, d) <- xs, c == a, notElem c ys] ++ 
                  concat [paths' c b (filter (\(x, y) -> x /= d && y /= d) xs) (ys ++ [d]) | (c, d) <- xs, d == a, notElem d ys]
-}


-- 82. Return all cycles from a starting node via backtracking
cycles :: (Eq a) => a -> [(a, a)] -> [[a]]
cycles n g = search [[n]] []
    -- If there are no more paths to explore (cur is empty), return the accumulated result.
    where search [] result = result
          -- Continue searching with updated active paths and accumulated results.
          search cur result = search (go active) (arrive ++ result)
            -- Split the current paths into those that have reached the end (arrive) and those that are still active.
            where split = partition end cur
                  -- A path has reached the end if it ends at 'n' and its length is not 1 (to avoid trivial cycles).
                  end s = (last s == n) && (length s /= 1)
                  -- Active paths are those that have not yet completed a cycle.
                  active = snd split
                  -- Arrived paths are those that have completed a cycle.
                  arrive = fst split
                  -- Extend each active path by adding a new node if it forms a valid edge and does not revisit nodes in the path.
                  go ls = [ x ++ [snd y] | x <- ls, y <- g, last x == fst y, not (snd y `elem` tail x)]


-- 83. Construct all spanning trees with backtracking (equivalently, construct all partial graphs with n nodes and (n-1) edges that are trees).

-- Check if there are exactly n-1 edges and if the graph is connected
isSpanningTree :: (Eq a) => [a] -> [(a, a)] -> Bool
isSpanningTree nodes edges = length edges == length nodes - 1 && isConnected nodes edges

-- Check if the graph is connected by exploring the nodes and checking if all nodes are visited
isConnected :: (Eq a) => [a] -> [(a, a)] -> Bool
isConnected nodes edges =
  let start = head nodes
      visited = bfs [start] edges []
  in length visited == length nodes


bfs :: (Eq a) => [a] -> [(a, a)] -> [a] -> [a]
bfs [] _ visited = visited -- No more nodes to explore
bfs (x:xs) edges visited
  | x `elem` visited = bfs xs edges visited -- Skip visited nodes
  | otherwise =
    -- Add neighbors to the queue and continue exploring
      let neighbors = [b | (a, b) <- edges, a == x] ++ [a | (a, b) <- edges, b == x]
      in bfs (xs ++ neighbors) edges (x : visited)

-- Generate with backtracking all possible spanning trees from subsequences of length (n-1) of the edges
spanningTree :: (Eq a) => Graph a -> [[(a, a)]]
spanningTree (Graph nodes edges) =
  filter (isSpanningTree nodes) (subsequences edges)


-- 84. Construct the minimal spanning tree of a weighted graph. (using prim's algorithm)
-- Funcția auxiliară de implementare
prim' :: (Eq a, Ord a) => [a] -> [(a, a, Int)] -> [a] -> [(a, a, Int)] -> [(a, a, Int)]
prim' [] _ _ mst = mst -- All nodes have been included in the MST
prim' nodes edges included mst 
  | null possibleEdges = mst -- No more edges to explore
  | otherwise = 
    -- Add the next edge to the MST and continue exploring
      let nextEdge = minimumBy (\(_, _, w1) (_, _, w2) -> compare w1 w2) possibleEdges
          (_, newNode, _) = nextEdge
      in prim' (filter (/= newNode) nodes) edges (newNode : included) (nextEdge : mst)
  where
    -- Filter the edges that connect the included nodes to the excluded nodes
    possibleEdges = filter (\(x, y, _) -> (x `elem` included && not (y `elem` included)) || 
                                          (y `elem` included && not (x `elem` included))) edges

prim :: (Eq a, Ord a) => [a] -> [(a, a, Int)] -> [(a, a, Int)]
prim nodes edges = prim' nodes edges [head nodes] []

-- 85. Graph isomorphism
graphG1 :: Graph Int
graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]

graphH1 :: Graph Int
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

permuteEdges :: (Eq a) => [a] -> [a] -> [(a, a)] -> [(a, a)]
permuteEdges nodes perm edgesToPermute =
  [(mapNode x, mapNode y) | (x, y) <- edgesToPermute]
  where
    -- Map a node to its new position in the permutation
    mapNode node = case lookup node (zip nodes perm) of
                     Just newNode -> newNode
                     Nothing -> node

-- Generate all permutations of the nodes and permute the edges accordingly
perms :: (Eq a) => Graph a -> [Graph a]
perms (Graph nodes edges) =
  [ Graph nodes (permuteEdges nodes perm edges) | perm <- permutations nodes ]

-- Check if two graphs are equal
eq :: (Eq a, Ord a) => Graph a -> Graph a -> Bool
eq (Graph nodes1 edges1) (Graph nodes2 edges2) = nodes1 == nodes2 && (sort edges1) == (sort edges2)

iso :: (Eq a, Ord a) => Graph a -> Graph a -> Bool
iso g1 g2 = any (eq g2) (perms g1)


-- 86. Node degree and graph coloration
-- a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node. 
-- b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.
-- c) Use Welch-Powell's algorithm to color the nodes of a graph.

-- a) Degree of a node
degree :: (Eq a) => Graph a -> a -> Int
degree (Graph _ edges) node = length [x | (x, _) <- edges, x == node] + length [y | (_, y) <- edges, y == node]

-- b) Sort nodes by degree
sortByDegree :: (Eq a) => Graph a -> [a]
sortByDegree graph@(Graph nodes _) = 
    map fst $ sortBy (\(_, d1) (_, d2) -> compare d2 d1) [(node, degree graph node) | node <- nodes]

-- c) Graph coloration
{- 
    "The Welch – Powell algorithm is used to color a graph, by sorting all vertices based on their 
    degrees from the largest degree to the smallest degree, then taking the first color (eg red),
     coloring the first vertex that we have sorted by degree earlier."
-}
kColor :: (Eq a) => [a] -> [(a, a)] -> [(a, Int)]
kColor nodes edges = kColor' nodes edges (sortByDegree (Graph nodes edges)) []

kColor' :: (Eq a) => [a] -> [(a, a)] -> [a] -> [(a, Int)] -> [(a, Int)]
kColor' [] _ _ colors = colors -- All nodes have been colored
kColor' (node:nodes) edges sortedNodes colors =
  -- Get the neighbors of the current node and their colors
  let neighbors = [y | (x, y) <- edges, x == node] ++ [x | (x, y) <- edges, y == node]
      -- Get the colors of the neighbors
      neighborColors = [color | (node, color) <- colors, node `elem` neighbors]
      -- Get the first available color
      color = head [c | c <- [1..], not (c `elem` neighborColors)]
  -- Continue coloring the rest of the nodes
  in kColor' nodes edges sortedNodes ((node, color) : colors)


-- 87. Depth-first order graph traversal
depthFirst :: (Eq a) => ([a], [(a, a)]) -> a -> [a]
depthFirst (_, edges) start = dfs start [] []
  where
    -- Depth-first search
    dfs current visited result
      -- If the current node has already been visited, return the result
      | current `elem` visited = result
      -- Otherwise, continue exploring the neighbors
      | otherwise = dfsHelper (current : visited) (result ++ [current]) (getNeighbors current)
    
    -- Helper function to explore the neighbors of a node
    dfsHelper _ result [] = result -- No more neighbors to explore
    dfsHelper visited result (n:ns) -- Explore the neighbors of the current node
      -- Skip visited nodes
      | n `elem` visited = dfsHelper visited result ns
      -- Continue exploring the neighbors
      | otherwise = dfsHelper (n : visited) (result ++ [n]) (getNeighbors n ++ ns)

    -- Get the neighbors of a node
    getNeighbors node = [y | (x, y) <- edges, x == node] ++ [x | (x, y) <- edges, y == node]


-- 88. Connected components (using dfs for every component)
-- Get the connected components of a graph
connectedComponents' :: (Eq a) => [a] -> [(a, a)] -> [[a]] -> [[a]]
connectedComponents' [] _ result = result -- All nodes have been visited
connectedComponents' (x:xs) edges result
  -- If the current node is part of a connected component, skip it
  | any (x `elem`) result = connectedComponents' xs edges result
  -- Otherwise, explore the connected component
  | otherwise = connectedComponents' xs edges (component : result)
  where
    component = depthFirst (x:xs, edges) x

connectedComponents :: (Eq a) => ([a], [(a, a)]) -> [[a]]
connectedComponents (nodes, edges) = connectedComponents' nodes edges []


-- 89. Bipartite graphs (equivalenty to check if the graph admits a 2-coloring)
-- BFS of a graph by mapping a node to a color
bfs' :: (Eq a) => [(a, Int)] -> [(a, a)] -> [(a, Int)] -> [(a, Int)]
bfs' [] _ colorMap = colorMap -- All nodes have been colored
bfs' ((node, color):queue) edges colorMap -- Explore the neighbors of the current node
  -- Skip visited nodes
  | node `elem` map fst colorMap = bfs' queue edges colorMap
  -- Continue exploring the neighbors
  | otherwise = bfs' (queue ++ neighbors) edges ((node, color) : colorMap)
  where
    -- Get the neighbors of the current node
    neighbors = [(y, 1 - color) | (x, y) <- edges, x == node] ++ [(x, 1 - color) | (x, y) <- edges, y == node]


bfsColor' :: (Eq a) => [a] -> [(a, a)] -> [(a, Int)] -> [a] -> [(a, Int)]
bfsColor' [] _ colorMap _ = colorMap -- All nodes have been colored
bfsColor' (n:ns) edges colorMap visited -- Explore the neighbors of the current node
  -- Skip visited nodes
  | n `elem` visited = bfsColor' ns edges colorMap visited
  -- Continue exploring the neighbors
  | otherwise = bfsColor' ns edges (colorMap ++ newColors) (visited ++ newNodes)
  where
    newColors = bfs' [(n, 0)] edges colorMap
    newNodes = map fst newColors

-- Color the nodes of a graph using bfs
bfsColor :: (Eq a) => [a] -> [(a, a)] -> [(a, Int)]
bfsColor nodes edges = bfsColor' nodes edges [] []

-- Check if a graph is bipartite
bipartite :: (Eq a) => ([a], [(a, a)]) -> Bool
bipartite (nodes, edges) = all (\(x, y) -> color x /= color y) edges
  where
    colorMap = bfsColor nodes edges
    color node = case lookup node colorMap of
                   Just c -> c
                   Nothing -> error "Node not found in color map"
