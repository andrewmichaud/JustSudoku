-- Andrew Michaud
-- 2/11/14
-- Main controller/logic for Sudoku project

module Main where

-- For Read.Maybe
import Text.Read

-- Command-line stuff
import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt( getOpt, usageInfo, ArgOrder(..) )

-- For maybes.
import Data.Maybe

import Data.List(nub)

-- Sudoku-related things
import Board
import Move
import Parse
import View
import Flag

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

-- Die if we received a QuitError (by using checkError to check the error).
maybeDie :: Either MoveError SudokuBoard -> IO ()
maybeDie val = do
    either (checkError) (const continue ) val
     
-- Die if passed a QuitError, otherwise do nothing.
checkError :: MoveError -> IO ()
checkError QuitError = die
checkError _         = continue

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
move _ (Quit) = Left $ QuitError

-- Command-line stuff
exit :: IO ()
exit = exitWith ExitSuccess

die :: IO ()
die = exitWith $ ExitFailure 1

header :: String
header = "Usage: sudoku-linux [-hgfV] [file]"

versionNum :: String
versionNum = "0.7.0.0"

version :: String
version = "Sudoku-Linux version " ++ versionNum

-- Does nothing useful, but returns an IO ().
continue :: IO ()
continue = do
    return ()

-- Parse arguments
commandParse :: ([Flag], [String], [String]) -> IO ([Flag], [String])

commandParse (args, fs, [])
    | Help `elem` args    = do
        hPutStrLn stderr (usageInfo header options)
        exitWith ExitSuccess
    
    | Version `elem` args = do
        hPutStrLn stderr version
        exitWith ExitSuccess
    
    | otherwise           = do
        let files = if null fs then ["-"] else fs
        return $ (nub args, files)


-- Failing out.    
commandParse (_, _, errs)   = do
    hPutStrLn stderr (concat errs ++ ( usageInfo header options) )
    exitWith (ExitFailure 1)

main :: IO ()
main = do

    -- Command line arguments.
    arguments <- getArgs
    
    let argTriple = getOpt Permute options arguments

    (args, _) <- commandParse argTriple
    
    -- Read from file.
    contents <- readFile "gamefiles/easy1.sfile"

    -- Process arguments.
    if Graphical `elem` args
        
        -- Graphical branch.
        then do
            -- Initialize and retrieve GUI.
            window <- initSudokuView
            runSudokuWindow window

        -- Command line branch.
        else do
            let board = attemptLoad contents
    
            -- Game header.
            putStrLn $ "This is Sudoku-Linux version " ++ versionNum ++ " \n"

            -- Print original board.
            putStrLn "This is the board\n"

            putStrLn $ prettyPrint board

            -- Print prompt.
            putStrLn "What would you like to do?\n"
            
            -- Enter control loop.
            repl move parseMove board

            exit

