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
repl :: (SudokuBoard -> Move -> Either MoveError SudokuBoard) -> (String -> Maybe Move) -> SudokuBoard -> IO ()
repl f parse init = do
    
    -- Grab input line.
    line <- getLine

    -- Parse the line as a move.
    let possibleMove = parseMove line

    -- This handles the cases of failing to parse and successfully (maybe) parsing.
    maybe fail succeed (parse line) where

        -- If we fail to parse, say so and recurse with init.
        fail = do
            putStrLn "Unable to parse move. Try again.\n"
            repl f parse init

        -- If nothing immediately goes wrong, we take the a and attempt to apply it
        -- to receive a new state.
        succeed a = do

            -- Obtain and print newstate.
            let newstate  = f init a
            putStrLn $ either show prettyPrint newstate

            -- Print prompt.
            putStrLn "What would you like to do?\n"

            -- either has type (a -> c) -> (b -> c) -> Either a b -> c
            -- I'm using the Left value of Either as an error, and the right value as an actual
            -- value.  So, here we do the following.
            -- If newstate is an error, we return init. const init is a function that takes a value
            -- and throws it away, and returns init.
            -- If newstate is not an error, we return it. id just takes newstate and returns it.
            -- The end result is that we have a new value to recurse with repl.
            repl f parse $ either (const init) id newstate

-- Parse a move
-- Also styled after a friend's code.
parseMove :: String -> Maybe Move

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
                 OtherError String | InvalidBoardError [(Location, Location)]

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

-- Set value to board. If an error occurs, the value will not be set. If no error occurs, the
-- requested value will be set.
move board (Set row col val)
    | rowInt   == Nothing = Left $ NaNError row
    | colInt   == Nothing = Left $ NaNError col
    | valInt   == Nothing = Left $ NaNError val
    | location == Nothing = Left $ OutOfBoundsError (fromJust rowInt) (fromJust colInt)
    | value    == Nothing = Left $ InvalidValueError (fromJust valInt)
    | otherwise           = Right $ newBoard
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
    
    -- Game header.
    putStrLn "This is Sudoku-Linux version 0.2\n"

    -- Print original board.
    putStrLn "This is the board\n"

    let board   = emptyBoard
        
    putStrLn $ prettyPrint board

    -- Print prompt.
    putStrLn "What would you like to do?\n"
    
    -- Enter control loop.
    repl move parseMove board

