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
emptyBoard = SudokuBoard [replicate 9 x | x <- (replicate 9 Nothing)]

-- Get the value in a particular square of a Sudoku board.
getBoardValue :: SudokuBoard -> Int -> Int -> SquareValue
getBoardValue (SudokuBoard board) rowIndex colIndex = value
    where
        row   = board !! rowIndex
        value = row !! colIndex

-- Returns a SudokuBoard with the value at the two indexes modified.
-- setBoardValue :: SudokuBoard -> Int -> Int -> Int -> SudokuBoard
-- setBoardValue (SudokuBoard board) rowIndex colIndex newValue =
