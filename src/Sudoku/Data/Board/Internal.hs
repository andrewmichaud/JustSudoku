{-|

Module      : Board
Description : Methods and datatypes for creating and manipulating Sudoku Boards.
Copyright   : (c) Andrew Michaud, 2015
License     : BSD3
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module includes the methods to create a SudokuBoard, the types used to construct one, and
all of the methods used to interact with one once it's created.

-}

module Sudoku.Data.Board.Internal where

import Data.Maybe
import Data.List

import Util.Other

-- Types declared.

-- | Location on a SudokuGrid - a coordinate pair.
data Location -- | Create location given row and column.
              = Loc Int Int deriving (Eq)

-- | Type for Sudoku square value.  Stores value and whether this square was placed at
--   creation of the board or not.
data Square = Empty                  -- ^ An empty square.
            |  Val { value :: SqVal  -- ^ Value of square.
                   , isOrig  :: Bool -- ^ Did this square come with the board?
                   } deriving (Eq)

-- | Value of a Square, can be 1-9
data SqVal = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 -- ^ Constructors for values 1-9.
             deriving (Eq, Ord, Enum, Show)

-- | Type for Sudoku board.  Just a 2D list of squares.
data SudokuBoard = SudokuBoard [[Square]] deriving (Eq, Show) -- ^ Create board from 2D list.

-- Methods visible from this module.

-- | Return whether Square is Empty or not.
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Convert Int to Square (or fail and return Nothing), with 'original' flag as specified.
toSquare :: Bool -> String -> Maybe Square
toSquare _ "0" = Just Empty
toSquare b "1" = squareGen b V1
toSquare b "2" = squareGen b V2
toSquare b "3" = squareGen b V3
toSquare b "4" = squareGen b V4
toSquare b "5" = squareGen b V5
toSquare b "6" = squareGen b V6
toSquare b "7" = squareGen b V7
toSquare b "8" = squareGen b V8
toSquare b "9" = squareGen b V9
toSquare b "_" = Just Empty
toSquare _  _  = Nothing

-- | Helper method for toSquare to cut down on code repetition.
squareGen :: Bool -> SqVal -> Maybe Square
squareGen b v = Just Val {value = v, isOrig = b}

-- | Create location from two strings specifying coordinates.
toLocation :: String -> String -> Maybe Location
toLocation rowStr colStr
    | isNothing r || isNothing c = Nothing
    | otherwise                  = Just $ Loc (fromJust r) (fromJust c)
    where
        r = toIndex rowStr
        c = toIndex colStr

-- | Convert tuple of ints to Location.
tupleToLocation :: (Int, Int) -> Location
tupleToLocation (r, c) = Loc r c

-- | Create an empty SudokuBoard.
emptyBoard :: SudokuBoard
emptyBoard = SudokuBoard [replicate 9 x | x <- replicate 9 Empty]

-- | Attempts to load a board from a file.
--   If it fails, an empty board is returned.
attemptLoad :: String -> SudokuBoard
attemptLoad fileContents = board
    where
        fileLines   = lines fileContents
        fileStrings = map words fileLines
        board       = createBoard fileStrings

-- | Reset board.  Values placed when the board was created are not changed.
resetBoard :: SudokuBoard -> SudokuBoard
resetBoard (SudokuBoard board) = SudokuBoard erased
    where erased = map (map eraseLocation) board

-- | Erase square if it isn't an original value.
eraseLocation :: Square -> Square
eraseLocation oldSquare
    | oldSquare == Empty       = oldSquare
    | isOrig oldSquare         = oldSquare
    | otherwise                = Empty

-- | Get the value in a particular square of a Sudoku board.
getBoardValue :: SudokuBoard -> Location -> Square
getBoardValue (SudokuBoard board) (Loc r c) = val
    where
        curRow = board !! r
        val    = curRow !! c

-- | Returns a SudokuBoard with the value at the two indices modified.
--   Original values are not modified.
setBoardValue :: SudokuBoard -> Location -> Square -> SudokuBoard
setBoardValue (SudokuBoard board) (Loc r c) newSquare = SudokuBoard newBoard
    where

        -- Create new row.
        oldRow   = board !! r
        oldValue = oldRow !! c
        newRow
            | oldValue == Empty = take c oldRow ++ [newSquare] ++ drop (c + 1) oldRow
            | isOrig oldValue   = oldRow
            | otherwise         = take c oldRow ++ [newSquare] ++ drop (c + 1) oldRow

        -- Create new board.
        newBoard  = take r board ++ [newRow] ++ drop (r + 1) board

-- | Erase square of a SudokuBoard if it isn't original.
eraseBoardValue :: SudokuBoard -> Location -> SudokuBoard
eraseBoardValue (SudokuBoard board) (Loc rowIndex colIndex) = SudokuBoard newBoard
    where oldRow   = board !! rowIndex
          oldValue = oldRow !! colIndex
          newRow
            | oldValue == Empty = oldRow
            | isOrig oldValue   = oldRow
            | otherwise         = take colIndex oldRow ++ [Empty] ++ drop (colIndex + 1) oldRow

          newBoard = take rowIndex board ++ [newRow] ++ drop (rowIndex + 1) board

-- | Checks if a SudokuBoard is currently valid or not.
--   Checks all rows, columns, and subgrids with checkList and returns a
--   list of squares in error.
checkBoard :: SudokuBoard -> [Location]
checkBoard sudokuBoard = allPairs
    where -- Get all kinds of pairs.
          rowPairs     = checkRows sudokuBoard
          colPairs     = checkCols sudokuBoard
          subgridPairs = checkSubgrids sudokuBoard

          -- Don't forget to remove duplicates within subgrids.
          allPairs     = (colPairs `union` subgridPairs) `union` (rowPairs `union` subgridPairs)

-- | Prints a SudokuBoard nicely.
prettyPrint :: SudokuBoard -> String
prettyPrint (SudokuBoard board) = niceBoard
    where -- Horizontal spacer line.
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

-- | Show either the number, or "_" for empty.
instance Show Square where
    show (Empty)          = "_"
    show (Val v _) = tail $ show v

-- | Show a location as a tuple of coordinates.
instance Show Location where
    show (Loc r c) = show (r, c)

-- | Turn an int into an index value.
toIndex :: String -> Maybe Int
toIndex str = sToIntRange str [0..8]

-- | Create board from array of array of string.  Used for loading from files.
createBoard :: [[String]] -> SudokuBoard
createBoard board = SudokuBoard values
    where maybes = map (map (toSquare True)) board
          values = map (map fromJust) maybes

-- | Check a list of lists of squares for duplicates
checkLocGrid :: [[Location]] -> SudokuBoard -> [Location]
checkLocGrid locations board = matchingLocations
    where squares           = map (map (getBoardValue board)) locations
          zipped            = zipLists locations squares
          matchingLocations = concatMap checkList zipped

-- | Returns a list of all locations that are invalid (for rows).
checkRows :: SudokuBoard -> [Location]
checkRows = checkLocGrid locations
    where locations = [ [ Loc r c | c <- [0..8] ] | r <- [0..8] ]

-- | Returns a list of all locations that are invalid (for columns).
checkCols :: SudokuBoard -> [Location]
checkCols = checkLocGrid locations
    where locations = [ [ Loc r c | r <- [0..8] ] | c <- [0..8] ]

-- | Returns a list of all locations that are invalid (for subgrids).
checkSubgrids :: SudokuBoard -> [Location]
checkSubgrids = checkLocGrid locations
    where locations = map subgridHelper [0..8]

-- | Takes a list of tuples of Int and Square. The Int is an indexing tag.
--   Returns a list of locations corresponding to matching squares.
checkList :: [(Location, Square)] -> [Location]
checkList [] = []
checkList (x:xs)
    | null (tail xs) = if checkPair (snd x) (snd $ head xs) then [fst x, fst $ head xs] else []
    | otherwise      = answer
    where matchingY    = [fst y | y <- xs, checkPair (snd x) (snd y)]
          headMatching = if not (null matchingY) then fst x : matchingY else []
          answer       = headMatching ++ checkList xs

-- | Check if a pair of Squares are equal.
checkPair :: Square -> Square -> Bool
checkPair (Val squareA _) (Val squareB _) = squareA == squareB
checkPair _ _ = False

-- | Helper function for check functions. Given two lists of lists, returns a list
--   of lists of pairs.
zipLists :: [[a]] -> [[b]] -> [[(a, b)]]
zipLists [] _ = []
zipLists _ [] = []
zipLists (a:as) (b:bs) = zip a b : zipLists as bs

-- | Given an Int indicating the index of a subgrid, returns a list of Locations matching that
--   subgrid.
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
-- Shouldn't ever get here, but ghc complained, so we cover this case anyway.
subgridHelper _ = []

-- | Given an array and a spacer, uses the spacer to separate
--   the first three from the middle three rows and the middle
--   three from the last three rows.
sudokuSpacer :: a -> [a] -> [a]
sudokuSpacer spacer array = result
    where first3 = take 3 array
          mid3   = take 3 (drop 3 array)
          last3  = drop 6 array
          result = first3 ++ [spacer] ++ mid3 ++ [spacer] ++ last3
