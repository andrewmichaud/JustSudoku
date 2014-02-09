-- Andrew Michaud
-- 02/05/15
-- Data and other definitions needed for Sudoku.

module SudokuBoard
( SudokuBoard
, Square
, SqVal
, emptyBoard
) where

import Data.Maybe
import qualified Data.List

data SqVal = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Eq, Ord, Enum, Show)

toVal :: Int -> Maybe SqVal
toVal 1 = Just V1
toVal 2 = Just V2
toVal 3 = Just V3
toVal 4 = Just V4
toVal 5 = Just V5
toVal 6 = Just V6
toVal 7 = Just V7
toVal 8 = Just V8
toVal 9 = Just V9
toVal _ = Nothing

-- Type for Sudoku square value.
data Square = Empty | Val SqVal deriving (Eq)

-- Show either the number, or "N" for nothing.
instance Show Square where
    show (Val value) = show value
    show (Empty)    = "N"

-- Type for Sudoku board.
data SudokuBoard = SudokuBoard [[Square]] deriving ()

-- Check if a pair of Squares are equal.
checkPair :: Square -> Square -> Bool
checkPair (Val squareA) (Val squareB) = (squareA == squareB)
checkPair _ _ = False

-- Get all pairs of values in a list.
allPairs :: [a] -> [(a, a)]
allPairs list
    | listLength == 2 = [(firstElem, lastElem)]
    | otherwise       = pairs
    where
        listLength = length list
        firstElem = head list
        lastElem = last list
        remainingElems = tail list
    
        -- Pairs in the list that don't use the first element.
        restPairs    = (allPairs remainingElems)
        restLength   = listLength - 1

        -- Generate a list of pairs including the first element.
        repeatedHead = replicate restLength head
        headPairs    = zip repeatedHead remainingElems

        -- Get a list of all pairs.
        pairs        = headPairs ++ restPairs

-- Check a list of Squares and returns a list of pairs that are equal.
--checkList :: [Square] -> [(Square, Square)]
--checkList squareList = answer
--    where
--        pairs = allPairs squareList
        


-- Checks that a row has no duplicate SquareValues.
--checkRow :: [SquareValue] -> (Bool, [String])
--checkRow row = findAndMarkDupes row

-- Checks if a SudokuBoard is currently valid or not.
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
emptyBoard = SudokuBoard [replicate 9 x | x <- (replicate 9 (Empty))]

-- Get the value in a particular square of a Sudoku board.
getBoardValue :: SudokuBoard -> Int -> Int -> Square
getBoardValue (SudokuBoard board) rowIndex colIndex
    
-- Check if out of bounds.
    | rowIndex > 8 || rowIndex < 0 = Empty
    | colIndex > 8 || colIndex < 0 = Empty
    
    -- Otherwise return value.
    | otherwise                    = value
    where
        row   = board !! rowIndex
        value = row !! colIndex

-- Returns a SudokuBoard with the value at the two indices erased.
eraseBoardValue :: SudokuBoard -> Int -> Int -> SudokuBoard
eraseBoardValue (SudokuBoard board) rowIndex colIndex
    -- Check if out of bounds.
    | rowIndex > 8 || rowIndex < 0 = SudokuBoard board
    | colIndex > 8 || colIndex < 0 = SudokuBoard board
    
    -- Check if we aren't actually changing a value.
    | oldSquare == Empty           =  SudokuBoard board

    -- Return new board if everything seems all right.
    | otherwise                    = SudokuBoard newBoard
    where
        -- Grab the row we care about modifying.
        -- Also grab square from that row.
        rowToMod  = board !! rowIndex
	oldSquare = rowToMod !! colIndex 

        -- Create new row.
        newRow    = take colIndex rowToMod ++ [Empty] ++ drop (colIndex + 1) rowToMod

	-- Create new board.
        newBoard  = take rowIndex board ++ [newRow] ++ drop (colIndex + 1) board

-- Returns a SudokuBoard with the value at the two indices modified.
setBoardValue :: SudokuBoard -> Int -> Int -> Int -> SudokuBoard
setBoardValue (SudokuBoard board) rowIndex colIndex newValue 
    
    -- Check if out of bounds.
    | rowIndex > 8 || rowIndex < 0 = SudokuBoard board
    | colIndex > 8 || colIndex < 0 = SudokuBoard board
    
    -- Check if we aren't actually changing a value.
    | oldSquare == newSquare       =  SudokuBoard board

    -- Return new board if everything seems all right.
    | otherwise                    = SudokuBoard newBoard
    where
        -- Create new square.
        maybeVal  = toVal newValue
        newSquare = if (isNothing maybeVal) then oldSquare else Val (fromJust maybeVal)

        -- Grab the row we care about modifying.
        -- Also grab square from that row.
        rowToMod  = board !! rowIndex
	oldSquare = rowToMod !! colIndex 

        -- Create new row.
        newRow    = take colIndex rowToMod ++ [newSquare] ++ drop (colIndex + 1) rowToMod

	-- Create new board.
        newBoard  = take rowIndex board ++ [newRow] ++ drop (colIndex + 1) board
        



