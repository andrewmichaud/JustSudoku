-- Main controller/logic/program
-- Andrew Michaud
-- 2/11/14

module Main where

-- For Read.Maybe
import Text.Read

-- Command-line stuff
import System.Environment
import System.Exit

-- For maybes.
import Data.Maybe

-- Sudoku-related things
import Board
import Move
import Parse
import View

-- Styled (with permission) after a friend's code.
repl :: (SudokuBoard -> Move -> Either MoveError SudokuBoard) -> (String -> Maybe Move) -> SudokuBoard -> IO ()
repl f parse initial = do
    
    -- Grab input line.
    line <- getLine

    -- Parse the line as a move.
    --let possibleMove = parseMove line

    -- This handles the cases of failing to parse and successfully (maybe) parsing.
    maybe failAction succeedAction (parse line) where

        -- If we fail to parse, say so and recurse with init.
        failAction = do
            putStrLn "Unable to parse move. Try again.\n"
            repl f parse initial

        -- If nothing immediately goes wrong, we take the a and attempt to apply it
        -- to receive a new state.
        succeedAction a = do

            -- Obtain and print newstate.
            let newstate  = f initial a
            putStrLn $ either show prettyPrint newstate
            
            -- Check if we were told to quit.
            maybeDie newstate

            -- Print prompt.
            putStrLn "What would you like to do?\n"

            -- either has type (a -> c) -> (b -> c) -> Either a b -> c
            -- I'm using the Left value of Either as an error, and the right value as an actual
            -- value.  So, here we do the following.
            -- If newstate is an error, we return init. const init is a function that takes a value
            -- and throws it away, and returns init.
            -- If newstate is not an error, we return it. id just takes newstate and returns it.
            -- The end result is that we have a new value to recurse with repl.
            repl f parse $ either (const initial) id newstate

maybeDie :: Either MoveError SudokuBoard -> IO ()
maybeDie val = do
    either (const die) (const continue ) val
     
continue :: IO ()
continue = do
    return ()

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

-- Erase requested value. If an error occurs, the value will not be erased.
move board (Erase row col)
    | rowInt   == Nothing = Left $ NaNError row
    | colInt   == Nothing = Left $ NaNError col
    | location == Nothing = Left $ OutOfBoundsError (fromJust rowInt) (fromJust colInt)
    | otherwise           = Right $ newBoard
    where
        location = parseLocation row col
        rowInt   = readMaybe row
        colInt   = readMaybe col
        newBoard = eraseBoardValue board (fromJust location)

-- Check if any squares are invalid. "Error" and show invalid squares if any exist. 
-- In any case, the original board will remain.
move board (Check)
    | (length invalidSquares) > 0 = Left $ InvalidBoardError invalidSquares
    | otherwise                   = Right $ board
    where
        invalidSquares = checkBoard board

-- Reset board
move board (Reset) = Right $ resetBoard board

-- Unimplemented.
move board (Quit) = Left $ QuitError

-- Command-line stuff
exit :: IO ()
exit = exitWith ExitSuccess

die :: IO ()
die = exitWith $ ExitFailure 1

main :: IO ()
main = do

    -- Command line arguments.
    arguments <- getArgs
    print arguments

    -- Initialize and retrieve GUI.
    window <- initSudokuView
    
    -- Read from file.
    contents <- readFile "gamefiles/easy1.sfile"

    let board = attemptLoad contents
    
    runSudokuWindow window

    -- Game header.
    putStrLn "This is Sudoku-Linux version 0.4\n"

    -- Print original board.
    putStrLn "This is the board\n"

    putStrLn $ prettyPrint board

    -- Print prompt.
    putStrLn "What would you like to do?\n"
    
    -- Enter control loop.
    repl move parseMove board

    exit

