-- Andrew Michaud
-- 02/05/15
-- Data and other definitions needed for Sudoku.

module SudokuBoard
( SudokuBoard
, SquareValue
, Position(..)
, emptyBoard
) where

import Prelude hiding (Nothing)
import qualified Data.List

-- Position type to make selecting a position easier.
data Position = Position {row::Int,  col::Int} deriving (Eq, Show)

    
-- Type for Sudoku square value.
data SquareValue = Number Int | Nothing deriving (Eq)

-- Show either the number, or "N" for nothing.
instance Show SquareValue where
    show (Number num) = show num
    show (Nothing)    = "N"

-- Type for Sudoku board.
data SudokuBoard = SudokuBoard [[SquareValue]] deriving ()

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

-- Show for SudokuBoard, prints it nicely.
instance Show SudokuBoard where
    show (SudokuBoard board) = niceBoard
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
 

-- Creates empty Sudoku board.
emptyBoard :: SudokuBoard
emptyBoard = SudokuBoard [replicate 9 x | x <- (replicate 9 (Nothing))]

-- Get the value in a particular square of a Sudoku board.
getBoardValue :: SudokuBoard -> Int -> Int -> Maybe SquareValue
getBoardValue (SudokuBoard board) rowIndex colIndex
    -- Check if out of bounds.
    | rowIndex > 8 || rowIndex < 0 = Just Nothing
    | colIndex > 8 || colIndex < 0 = Just Nothing
    -- Otherwise return value.
    | otherwise    = Just value
    where
        row   = board !! rowIndex
        value = row !! colIndex

-- Returns a SudokuBoard with the value at the two indexes modified.
setBoardValue :: SudokuBoard -> Int -> Int -> Int -> SudokuBoard
setBoardValue (SudokuBoard board) rowIndex colIndex newValue 
    -- Check if out of bounds.
    | rowIndex > 8 || rowIndex < 0 = SudokuBoard board
    | colIndex > 8 || colIndex < 0 = SudokuBoard board
    -- Check if we aren't actually changing a value.
    | oldValue == Number newValue  = SudokuBoard board
    | otherwise                    = SudokuBoard newBoard
    where
	newBoard  = take rowIndex board ++ [newRow] ++ drop (colIndex + 1) board
        rowToMod  = board !! rowIndex
        newRow    = take colIndex rowToMod ++ [Number newValue] ++ drop (colIndex + 1) rowToMod
	oldValue  = rowToMod !! colIndex 


