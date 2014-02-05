-- Andrew Michaud
-- 02/05/15
-- Data and other definitions needed for Sudoku.

module SudokuBoard
( SudokuBoard
, SquareValue
, emptyBoard
) where

import Prelude hiding (Nothing)

-- Type for Sudoku square value.
data SquareValue = Number Int | Nothing deriving (Eq, Show)

-- Type for Sudoku board.
data SudokuBoard = SudokuBoard [[SquareValue]] deriving (Show)

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
    | rowIndex > 8 || rowIndex < 0 = SudokuBoard board
    | colIndex > 8 || colIndex < 0 = SudokuBoard board
    | oldValue == newValue         = SudokuBoard board
    where
	row	 = board !! rowIndex
	oldValue = row !! colIndex 

instance Show SudokuBoard board where
    SudokuBoard board = newBoard

