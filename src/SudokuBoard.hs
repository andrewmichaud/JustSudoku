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
, prettyPrint
, getBoardValue
, setBoardValue
, checkBoard
) where

import Data.Maybe
import Text.Read

-- Types declared.

-- Location on a SudokuGrid
data Location = Loc Int Int deriving (Eq, Show)

-- Type for Sudoku square value.
data Square = Empty | Val SqVal deriving (Eq)

-- Value of a Square, can be 1-9
data SqVal = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Eq, Ord, Enum, Show)

-- Type for Sudoku board.
data SudokuBoard = SudokuBoard [[Square]] deriving (Show)

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

-- Get the value in a particular square of a Sudoku board.
getBoardValue :: SudokuBoard -> Location -> Maybe Square
getBoardValue (SudokuBoard board) (Loc rowIndex colIndex)
    
    -- Otherwise return value
    | otherwise                    = Just value
    where
        row   = board !! rowIndex
        value = row !! colIndex

-- Returns a SudokuBoard with the value at the two indices modified.
setBoardValue :: SudokuBoard -> Location -> Square -> Maybe SudokuBoard
setBoardValue (SudokuBoard board) (Loc rowIndex colIndex) newSquare
    
    -- Return new board if everything seems all right.
    | otherwise                    = Just $ SudokuBoard newBoard
    where
        
        -- Create new row.
        oldRow    = board !! rowIndex
        newRow    = take colIndex oldRow ++ [newSquare] ++ drop (colIndex + 1) oldRow

	-- Create new board.
        newBoard  = take rowIndex board ++ [newRow] ++ drop (colIndex + 1) board

-- Checks if a SudokuBoard is currently valid or not.
-- Checks all rows, columns, and subgrids with checkList and returns a 
-- list of matching pairs.
checkBoard :: SudokuBoard -> [(Square, Square)]
checkBoard (SudokuBoard board) = allPairs
    where
        rowPairs = concat $ map checkList board
        columns  = [getColumn board index | index <- [0..9]]
        colPairs = concat $ map checkList board
        allPairs = colPairs ++ rowPairs

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

-- Show either the number, or "N" for nothing.
instance Show Square where
    show (Val value) = show value
    show (Empty)    = "N"

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

-- Check a list of Squares and returns a list of pairs that are equal.
checkList :: [Square] -> [(Square, Square)]
checkList (x:xs)
    | tail xs == [] = if checkPair x (head xs) then [(x, (head xs))] else []
    | otherwise     = answer
    where
        headPairs   = [(x, y) | y <- xs, checkPair x y]
        answer      = headPairs ++ checkList xs
        
-- Retrieves a specified column from a 2D list.
getColumn :: [[a]] -> Int -> [a]
getColumn list index = [row !! index| row <- list]

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
        
