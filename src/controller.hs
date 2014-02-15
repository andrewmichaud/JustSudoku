-- Main controller/logic/program
-- Andrew Michaud
-- 2/11/14

module Main where

-- For argument processing.
import System.Environment

-- For file and other IO.
import System.IO

-- For custom errors.
import Control.Monad.Error

-- For Read.Maybe
import Text.Read


-- For maybes.
import Data.Maybe

import SudokuBoard
import Move

-- Styled (with permission) after a friend's code.
repl :: (Show a, Show c) => (a -> b -> Either a c) -> (String -> Maybe b) -> a -> IO ()
repl f parse init = do
    
    -- Grab input line.
    line <- getLine
    let possibleMove = parseMove line

    maybe fail succeed (parse line) where
        fail = do
            print "Parse error. Try again."
            repl f parse init
        succeed a = do
            let newstate = f init a
            print newstate
            repl f parse $ either id (const init) newstate

-- Parse a move
-- Also styled after a friend's code.
parseMove :: String -> Maybe Move

-- Get value at a row and column.
parseMove ('g':r:c)
    | loc == Nothing = Nothing
    | otherwise      = Just $ Get [r] c
    where
        loc = parseLocation [r] c

-- Set value at row and column to value.
parseMove ('s':r:c:v)
    | loc == Nothing = Nothing
    | val == Nothing = Nothing
    | otherwise      = Just $ Set [r] [c] v
    where
        loc = parseLocation [r] [c]
        val = parseSquare v

-- Check if board is valid.
parseMove "c" = Just Check

-- Quit.
parseMove "q" = Just Quit

-- No other moves are valid.
parseMove _ = Nothing

-- Turn an Int into a square (or nothing).
-- 0 is turned into Empty.
parseSquare :: String -> Maybe Square
parseSquare intString = toSquare intString

-- Turn two ints into a Location
parseLocation :: String -> String -> Maybe Location
parseLocation rowString colString = toLocation rowString colString

-- TODO please move this elsewhere.
-- Still borrowing from friend.
data MoveError = NaNError String | OutOfBoundsError Int Int | InvalidValueError Int | 
                 OtherError String | InvalidBoardError [(Square, Square)]

-- For when the error has an error?
instance Error MoveError where
    noMsg  = OtherError "KERNEL PANIC"
    strMsg = OtherError
instance Show MoveError where
    show (NaNError value)            = "Value " ++ value ++ " is not a number."
    show (OutOfBoundsError row col)  = "Square (" ++ show row ++ ", " ++ show col 
                                                 ++ " is out of bounds."
    show (InvalidValueError value)   = "Value " ++ show value ++ " is invalid."
    show (InvalidBoardError squares) = "Board is invalid. Invalid squares: " ++ show squares
    show (OtherError string)         = "General error: " ++ string

-- Try to processs a move.
move :: SudokuBoard -> Move -> Either MoveError SudokuBoard

-- Get the value. Return error if desired value is out of bounds or not a number.
-- In no event modify the board.
move board (Get row col)
    | rowInt   == Nothing = Left $ NaNError row
    | colInt   == Nothing = Left $ NaNError col
    | location == Nothing = Left $ OutOfBoundsError (fromJust rowInt) (fromJust colInt)
    | otherwise           = Right $ board
    where
        location = parseLocation row col
        rowInt   = readMaybe row
        colInt   = readMaybe col

-- Set value to board. If an error occurs, the value will not be set. If no error occurs, the
-- requested value will be set.
move board (Set row col val)
    | rowInt   == Nothing = Left $ NaNError row
    | colInt   == Nothing = Left $ NaNError col
    | valInt   == Nothing = Left $ NaNError val
    | location == Nothing = Left $ OutOfBoundsError (fromJust rowInt) (fromJust colInt)
    | value    == Nothing = Left $ InvalidValueError (fromJust valInt)
    | newBoard == Nothing = Left $ OtherError "lazy"
    | otherwise           = Right $ fromJust newBoard
    where
        location = parseLocation row col
        value    = parseSquare val
        rowInt   = readMaybe row
        colInt   = readMaybe col
        valInt   = readMaybe val
        newBoard = setBoardValue board (fromJust location) (fromJust value)

-- Check if any squares are invalid. "Error" and show invalid squares if any exist. 
-- In any case, the original board will remain.
move board (Check)
    | (length invalidSquares) > 0 = Left $ InvalidBoardError invalidSquares
    | otherwise                   = Right $ board
    where
        invalidSquares = checkBoard board

-- Unimplemented.
--move board (Quit)
--    | False == True = Left $ OtherError "???"
--    | otherwise     = Right $ board

main = do

    -- Command line arguments.
    arguments <- getArgs

    -- Read from file.
    --contents <- readFile filename
    

    let board   = emptyBoard
        pp      = prettyPrint board

    putStrLn pp

    --repl move parseMove board

