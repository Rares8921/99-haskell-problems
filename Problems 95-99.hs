{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Char -- 96.
import Data.List -- 98, 99
import Control.Monad (guard) -- 99.
import Data.Ord (comparing) -- 99.
import Data.Function (on) -- 99.


main :: IO ()
main = do
    putStrLn ""
    -- 95.
    print(fullWords 175)
    putStrLn ""
    -- 96.
    print(identifier "this-is-a-long-identifier")
    print(identifier "this-ends-in-")
    print(identifier "two--hyphens")
    putStrLn ""
    -- 97.
    print(sudoku [[0, 5, 0, 0, 6, 0, 0, 0, 1],
                  [0, 0, 4, 8, 0, 0, 0, 7, 0],
                  [8, 0, 0, 0, 0, 0, 0, 5, 2],
                  [2, 0, 0, 0, 5, 7, 0, 3, 0],
                  [0, 0, 0, 0, 0, 0, 0, 0, 0],
                  [0, 3, 0, 6, 9, 0, 0, 0, 5],
                  [7, 9, 0, 0, 0, 0, 0, 0, 8],
                  [0, 1, 0, 0, 0, 6, 5, 0, 0],
                  [5, 0, 0, 0, 3, 0, 0, 6, 0]]
          )
    putStrLn ""
    -- 98.
    nonogram [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
    putStrLn ""
    -- 99.
    print(solve $ readCrossword "ALPHA\nARES\nPOPPY\n\n  .  \n  .  \n.....\n  . .\n  . .\n    .\n")

-- 95. English number words 
-- Write a function that takes a number and returns it as a string in English words.
fullWords :: Int -> String
-- Use init to eliminate the last hyphen
fullWords n = init $ concatMap (\x -> case x of
    '0' -> "zero-"
    '1' -> "one-"
    '2' -> "two-"
    '3' -> "three-"
    '4' -> "four-"
    '5' -> "five-"
    '6' -> "six-"
    '7' -> "seven-"
    '8' -> "eight-"
    '9' -> "nine-") (show n)

-- 96. Syntax checker
-- Write a function that takes a string and returns True if it is a valid identifier.

identifier :: String -> Bool
identifier str = case str of
  (c:cs) -> isLetter c && checkHyphen cs -- First character must be a letter
  _      -> False -- Empty string is not valid
  where
    checkHyphen []         = True
    checkHyphen ('-':cs)   = checkAlphas cs
    checkHyphen cs         = checkAlphas cs
    
    checkAlphas []         = False
    checkAlphas (c:cs)     = isAlphaNum c && checkHyphen cs



-- 97. Sudoku solver: read a 2d int array and solve it (preferably backtracking, but any solution is fine;
-- for example, with flows/coupling)

type Sudoku = [[Int]]

-- Main function to solve the Sudoku puzzle
sudoku :: Sudoku -> Sudoku
sudoku grid = case solveSudoku grid of
  Just solution -> solution
  Nothing       -> grid

-- Recursive function to solve the Sudoku puzzle
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku grid = case findEmpty grid of
  Nothing -> Just grid -- No empty cells, puzzle solved
  Just (row, col) -> tryNumbers grid row col [1..9]

-- Find the first empty cell in the grid
findEmpty :: Sudoku -> Maybe (Int, Int)
findEmpty grid = case [(r, c) | r <- [0..8], c <- [0..8], grid !! r !! c == 0] of
  [] -> Nothing -- No empty cells found
  (x:_) -> Just x

-- Try numbers 1 to 9 in the empty cell and recursively solve the grid
tryNumbers :: Sudoku -> Int -> Int -> [Int] -> Maybe Sudoku
tryNumbers grid row col [] = Nothing
tryNumbers grid row col (n:ns)
  | isValid grid row col n = case solveSudoku (updateGrid grid row col n) of
    Just solution -> Just solution
    Nothing       -> tryNumbers grid row col ns
  | otherwise = tryNumbers grid row col ns

-- Check if placing a number in the cell is valid
isValid :: Sudoku -> Int -> Int -> Int -> Bool
isValid grid row col n = not (inRow grid row n || inCol grid col n || inBox grid row col n)

-- Check if the number is already in the row
inRow :: Sudoku -> Int -> Int -> Bool
inRow grid row n = n `elem` grid !! row

-- Check if the number is already in the column
inCol :: Sudoku -> Int -> Int -> Bool
inCol grid col n = n `elem` map (!! col) grid

-- Check if the number is already in the 3x3 box
inBox :: Sudoku -> Int -> Int -> Int -> Bool
inBox grid row col n = n `elem` [grid !! r !! c | r <- boxRange row, c <- boxRange col]

-- Get the range of indices for the 3x3 box
boxRange :: Int -> [Int]
boxRange x = let start = (x `div` 3) * 3 in [start..start+2]

-- Update the grid with the number in the specified cell
updateGrid :: Sudoku -> Int -> Int -> Int -> Sudoku
updateGrid grid row col n = take row grid ++ [take col (grid !! row) ++ [n] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid


-- 98. Nonogram solver: read a 2d int array and solve it (preferably backtracking, but any solution is fine);

newtype Square = Square Bool deriving (Eq)

-- A blank cell is shown as a space, and a filled cell is shown as 'X'.
instance Show Square where
  show (Square False) = " "
  show (Square True)  = "X"

-- blank represents an empty cell.
-- cross represents a filled cell.
blank, cross :: Square
blank = Square False
cross = Square True

-- Generates all possible rows of length 'n' that match the given block pattern.
-- The block pattern is a list of integers where each integer represents the length of a sequence of filled cells.
-- The function returns a list of possible rows that match the block pattern.
possibleRows :: Int -> [Int] -> [[Square]]
possibleRows n blocks = case blocks of
  [] -> [replicate n blank]
  (k:ks) | k > n -> []
      | otherwise -> [blank : row | row <- possibleRows (n-1) blocks] ++
              [replicate k cross ++ (if null ks then [] else blank : row) 
               | row <- possibleRows (n-k-1) ks]

-- Converts a row of Square's into a list of integers representing the lengths of sequences of filled cells.
compress :: [Square] -> [Int]
compress = map length . filter ((== Square True) . head) . group

-- Checks if the given rows match the target column patterns.
-- It transposes the rows and compresses them to compare with the target column patterns.
validSolution :: [[Square]] -> [[Int]] -> Bool
validSolution rows target = (map compress . transpose) rows == target

-- Attempts to solve the nonogram puzzle given the row and column patterns.
-- It returns a list of possible solutions, where each solution is a grid of 'Square's.
solveNonogram :: [[Int]] -> [[Int]] -> [[[Square]]]
solveNonogram horz vert = [sol | sol <- sequence (map (possibleRows (length vert)) horz), validSolution sol vert]

-- Prints the nonogram grid to the console.
display :: [[Square]] -> IO ()
display = putStrLn . unlines . map (concatMap show)

-- Is the main function to solve and display the nonogram puzzle.
-- It takes the row and column patterns as input and prints the solution if one is found.
-- If no solution is found, it prints "No solution found."
nonogram :: [[Int]] -> [[Int]] -> IO ()
nonogram horz vert = case solveNonogram horz vert of
  (solution:_) -> display solution
  _            -> putStrLn "No solution found."


-- 99. Crossword puzzle solver
type Coord     = (Int,Int)
data Site      = Site {siteCoords :: [Coord], siteLen :: Int} deriving (Show,Eq)
data Crossword = Crossword {cwWords :: [String], cwSites :: [Site]}  deriving (Show,Eq)

-- A helper that compares by a projection.
equaling :: Eq b => (a -> b) -> a -> a -> Bool
equaling f x y = f x == f y

-- Given raw grid lines, attach coordinates to each character.
indexed :: [String] -> [[(Coord, Char)]]
indexed xs = zipWith (\r line -> zip [(c, r) | c <- [1..]] line) [1..] xs

-- Break a list of (Coord,Char) into contiguous groups, then
-- filter each group so that only the dots remain. Finally, keep only groups longer than 1.
extractor :: [(Coord, Char)] -> [[(Coord, Char)]]
extractor = filter ((>1) . length)
      . map (filter ((== '.') . snd))
      . groupBy (equaling snd)

-- Build a Site from a group.
makeSite :: [(Coord, Char)] -> Site
makeSite pts = Site { siteCoords = map fst pts, siteLen = length pts }

-- Swap the coordinates of a Site (used for transposing).
swapSite :: Site -> Site
swapSite s = s { siteCoords = map (\(c,r) -> (r,c)) (siteCoords s) }

-- Convert raw grid lines into a list of Sites.
toSites :: [String] -> [Site]
toSites lines = find (index_it lines) ++ map swapSite (find (index_it (transpose lines)))
  where find       = map makePos . concatMap extractor
        extractor  = filter ((>1) . length) . map (filter ((=='.').snd)) . groupBy (equaling snd)
        index_it   = zipWith (\row -> zip [(row, col) | col <- [1..]]) [1..]
        makePos xs = Site {siteCoords = map fst xs, siteLen = length xs}

-- Pair each letter of a word with the corresponding coordinate in a Site.
together :: (String, Site) -> [(Coord, Char)]
together (w, s) = zip (siteCoords s) w

-- Ensure that if two assignments share a coordinate, they agree on the letter.
noCollision :: [(String, Site)] -> Bool
noCollision xs =
  let pairs = concatMap together xs
      groups = groupBy (\(c1, _) (c2, _) -> c1 == c2) pairs  -- assume pairs are already in order
  in all (\grp -> all (== snd (head grp)) (map snd grp)) groups

-- Backtracking solver: assign words to sites.
solve' :: [String] -> [Site] -> [[(String, Site)]]
solve' _ [] = [[]]
solve' ws (s:ss) =
  let poss = filter (\w -> length w == siteLen s) ws
  in if null poss 
     then error ("too few words of length " ++ show (siteLen s))
     else [ (w, s) : rest
      | w <- poss
      , rest <- solve' (delete w ws) ss
      , noCollision ((w, s) : rest)
      ]

-- The top-level solver returns a list of assignments.
solve :: Crossword -> [[(Coord, Char)]]
solve cw = map (concatMap together) (solve' (cwWords cw) (cwSites cw))

-- Read a file’s content into a Crossword.
readCrossword :: String -> Crossword
readCrossword input =
  let (ws, rest) = break (== "") (lines input)
      grid = drop 1 rest
  in Crossword ws (toSites grid)