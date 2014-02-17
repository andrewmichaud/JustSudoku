-- Andrew Michaud
-- 02/05/15
-- Data and other definitions needed for Sudoku.

module SudokuBoard 
( SqVal
, Square
, SudokuBoard
, Location
, toLocation
, toSquare
, emptyBoard
, loadBoard
, prettyPrint
, getBoardValue
, setBoardValue
, checkBoard
) where

import Data.Maybe
import Data.List

-- Types declared.

-- Location on a SudokuGrid
data Location = Loc {row :: Int, col :: Int} deriving (Eq)

-- Type for Sudoku square value.
data Square = Empty | Val SqVal deriving (Eq)

-- Value of a Square, can be 1-9
data SqVal = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Eq, Ord, Enum, Show)

-- Type for Sudoku board.
data SudokuBoard = SudokuBoard [[Square]] deriving (Eq, Show)

-- Methods visible from this module.

-- Convert Int to Square
toSquare :: String -> Maybe Square
toSquare "0" = Just Empty
toSquare "1" = Just $ Val V1
toSquare "2" = Just $ Val V2
toSquare "3" = Just $ Val V3
toSquare "4" = Just $ Val V4
toSquare "5" = Just $ Val V5
toSquare "6" = Just $ Val V6
toSquare "7" = Just $ Val V7
toSquare "8" = Just $ Val V8
toSquare "9" = Just $ Val V9
toSquare "_" = Just Empty
toSquare _ = Nothing

-- Create Location
toLocation :: String -> String -> Maybe Location
toLocation rowStr colStr 
    | row == Nothing || col == Nothing = Nothing
    | otherwise                        = Just $ Loc (fromJust row) (fromJust col)
    where
        row = toIndex rowStr
        col = toIndex colStr

-- Creates empty Sudoku board.
emptyBoard :: SudokuBoard
emptyBoard = SudokuBoard [replicate 9 x | x <- (replicate 9 (Empty))]

-- Load board from file.
loadBoard :: [[String]] -> SudokuBoard
loadBoard board = SudokuBoard values
    where
        maybes = map (map toSquare) board
        values = map (map fromJust) maybes

-- Get the value in a particular square of a Sudoku board.
getBoardValue :: SudokuBoard -> Location -> Square
getBoardValue (SudokuBoard board) (Loc rowIndex colIndex) = value
    where
        row   = board !! rowIndex
        value = row !! colIndex

-- Returns a SudokuBoard with the value at the two indices modified.
setBoardValue :: SudokuBoard -> Location -> Square -> SudokuBoard
setBoardValue (SudokuBoard board) (Loc rowIndex colIndex) newSquare = SudokuBoard newBoard
    where
        
        -- Create new row.
        oldRow    = board !! rowIndex
        newRow    = take colIndex oldRow ++ [newSquare] ++ drop (colIndex + 1) oldRow

	-- Create new board.
        newBoard  = take rowIndex board ++ [newRow] ++ drop (rowIndex + 1) board

-- Checks if a SudokuBoard is currently valid or not.
-- Checks all rows, columns, and subgrids with checkList and returns a 
-- list of matching pairs.
checkBoard :: SudokuBoard -> [(Location, Location)]
checkBoard sudokuBoard = allPairs
    where
        
        -- Get all kinds of paires.
        rowPairs       = checkRows sudokuBoard
        colPairs       = checkCols sudokuBoard
        subgridPairs   = checkSubgrids sudokuBoard
        -- Don't forget to remove duplicates within subgrids.
        allPairs       = union (union colPairs subgridPairs) (union rowPairs subgridPairs)

-- Show for SudokuBoard, prints it nicely.
prettyPrint :: SudokuBoard -> String
prettyPrint (SudokuBoard board) = niceBoard
    where
        -- Horizontal spacer line.
        horSepLine    = unwords (replicate 17 " ")
        
        -- Convert all SquareValues to strings.
        stringBoard   = map (map show) board
        
        -- Insert vertical spaces and then join lines into strings.
        vSpacedBoard  = map (sudokuSpacer " ") stringBoard
        unwordedBoard = map unwords vSpacedBoard
       
        -- Separate rows with horizontal space lines.
        separated     = sudokuSpacer horSepLine unwordedBoard
        
        -- Join array into newline-separated string.
        niceBoard     = unlines separated

-- Hidden in this module.

-- Show either the number, or "E" for empty.
instance Show Square where
    show (Val value) = tail $ show value
    show (Empty)    = "E"

instance Show Location where
    show (Loc row col) = show (row, col)

-- Turn an int into an index value.
toIndex :: String -> Maybe Int
toIndex "0" = Just 0
toIndex "1" = Just 1
toIndex "2" = Just 2
toIndex "3" = Just 3
toIndex "4" = Just 4
toIndex "5" = Just 5
toIndex "6" = Just 6
toIndex "7" = Just 7
toIndex "8" = Just 8
toIndex _   = Nothing

-- Check if a pair of Squares are equal.
checkPair :: Square -> Square -> Bool
checkPair (Val squareA) (Val squareB) = (squareA == squareB)
checkPair _ _ = False

-- Takes a list of tuples of Int and Square. The Int is an indexing tag.
-- Returns a list of pairs of indexing tags where the Square is equal.
checkList :: [(Location, Square)] -> [(Location, Location)]
checkList (x:xs)
    | tail xs == [] = if checkPair (snd x) (snd $ head xs) then [(fst x, (fst $ head xs))] else []
    | otherwise     = answer
    where
        headPairs   = [((fst x), (fst y)) | y <- xs, checkPair (snd x) (snd y)]
        answer      = headPairs ++ checkList xs

-- Helper function for check functions. Given two lists of lists, returns a list
-- of lists of pairs.
zipLists :: [[a]] -> [[b]] -> [[(a, b)]]
zipLists [] _ = []
zipLists _ [] = []
zipLists (a:as) (b:bs) = [zip a b] ++ zipLists as bs

-- Returns a list of all pairs of locations that are invalid (for rows).
checkRows :: SudokuBoard -> [(Location, Location)]
checkRows sudokuBoard = matchingLocations
    where
        locations         = [ [ Loc r c | c <- [0..8] ] | r <- [0..8] ]
        squares           = map (map (getBoardValue sudokuBoard)) locations
        zipped            = zipLists locations squares 
        matchingLocations = concat $ map checkList zipped

-- Returns a list of all pairs of locations that are invalid (for columns).
checkCols :: SudokuBoard -> [(Location, Location)]
checkCols sudokuBoard = matchingLocations
    where
        locations         = [ [ Loc r c | r <- [0..8] ] | c <- [0..8] ]
        squares           = map (map (getBoardValue sudokuBoard)) locations
        zipped            = zipLists locations squares 
        matchingLocations = concat $ map checkList zipped

-- Given an Int indicating the index of a subgrid, returns a list of Locations matching that
-- subgrid.
-- TODO find a better way to do this.
subgridHelper :: Int -> [Location]
subgridHelper 0 = [Loc r c | r <- [0..2], c <- [0..2]]
subgridHelper 1 = [Loc r c | r <- [0..2], c <- [3..5]]
subgridHelper 2 = [Loc r c | r <- [0..2], c <- [6..8]]
subgridHelper 3 = [Loc r c | r <- [3..5], c <- [0..2]]
subgridHelper 4 = [Loc r c | r <- [3..5], c <- [3..5]]
subgridHelper 5 = [Loc r c | r <- [3..5], c <- [6..8]]
subgridHelper 6 = [Loc r c | r <- [6..8], c <- [0..2]]
subgridHelper 7 = [Loc r c | r <- [6..8], c <- [3..5]]
subgridHelper 8 = [Loc r c | r <- [6..8], c <- [6..8]]

-- Returns a list of all pairs of locations that are invalid (for subgrids);
checkSubgrids :: SudokuBoard -> [(Location, Location)]
checkSubgrids sudokuBoard = matchingLocations
    where
        locations         = map subgridHelper [0..8]
        squares           = map (map (getBoardValue sudokuBoard)) locations
        zipped            = zipLists locations squares 
        matchingLocations = concat $ map checkList zipped

-- Given an array and a spacer, uses the spacer to separate
-- the first three from the middle three rows and the middle
-- three from the last three rows.
sudokuSpacer :: a -> [a] -> [a]
sudokuSpacer spacer array = result
    where 
        first3 = take 3 array
        mid3   = take 3 (drop 3 array)
        last3  = drop 6 array
        result = first3 ++ [spacer] ++ mid3 ++ [spacer] ++ last3
        
