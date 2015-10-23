{-|

Module      : Main
Description :  Handles user input and controlling everything else.
Copyright   : (c) Andrew Michaud, 2015
License     : BSD3
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module provides several things.  First, it provides a control loop for receiving and
interpreting player input.  This loop takes input, attempts to parse it (using Parse), gets
the Move (using Move) the player intended, applies it, and either loops with the new state
of the board after the move (if the move was valid), or loops with the old state of the board
(if there was an error).  It also shows the board on each iteration and kills the program if
a QuitError comes up (if the player asked to quit).

There are also functions for each possible move, to actually apply the player's moves.  If the
moves fail for any reason, these functions return errors instead of valid new boards.

The rest of the module is code to handle command line arguments, some strings holding info
about the program, and the actual Main function.  The main function parses arguments, prints
some stuff out, and kickstarts the control loop.

-}

module Main (

-- * Control
  repl
, move

-- * Command-Line Processing
, commandParse
, exit
, Main.die
, continue

-- * Program Information.
, header
, version

-- * Main
, main

) where

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
import qualified Data.Set as Set

-- Sudoku-related things
import Sudoku.Data.Board
import Sudoku.Control.Move
import Sudoku.Control.Parse
import Sudoku.Control.Flag

-- Styled after a friend's code.
-- | Control loop for Sudoku game. Receive user input, process it (handling errors) and repeat.
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

-- | Die if we received a QuitError (by using checkError to check the error).
maybeDie :: Either MoveError SudokuBoard -> IO ()
maybeDie = either checkError (const continue )

-- | Die if passed a QuitError, otherwise do nothing.
checkError :: MoveError -> IO ()
checkError QuitError = Main.die
checkError _         = continue

-- | Try to process a move.
move :: SudokuBoard -> Move -> Either MoveError SudokuBoard

-- Set value to board. If an error occurs, the value will not be set. If no error occurs, the
-- requested value will be set.
move board (Set row col val)
    | isNothing rowInt   = Left $ NaNError row
    | isNothing colInt   = Left $ NaNError col
    | isNothing valInt   = Left $ NaNError val
    | isNothing location = Left $ OutOfBoundsError (fromJust rowInt) (fromJust colInt)
    | isNothing newVal   = Left $ InvalidValueError (fromJust valInt)
    | otherwise          = Right newBoard
    where
        location = parseLocation row col
        newVal   = parseSquare val
        rowInt   = readMaybe row
        colInt   = readMaybe col
        valInt   = readMaybe val
        newBoard = setBoardValue board (fromJust location) (fromJust newVal)

-- Erase requested value. If an error occurs, the value will not be erased.
move board (Erase row col)
    | isNothing rowInt    = Left $ NaNError row
    | isNothing colInt    = Left $ NaNError col
    | isNothing location  = Left $ OutOfBoundsError (fromJust rowInt) (fromJust colInt)
    | otherwise           = Right newBoard
    where
        location = parseLocation row col
        rowInt   = readMaybe row
        colInt   = readMaybe col
        newBoard = eraseBoardValue board (fromJust location)

-- Check if any squares are invalid. "Error" and show invalid squares if any exist.
-- In any case, the original board will remain.
move board (Check)
    | not (Set.null invalidSquares) = Left $ InvalidBoardError invalidSquares
    | otherwise                 = Right board
    where
        invalidSquares = checkBoard board

-- Reset board.
move board (Reset) = Right $ resetBoard board

-- Quit the game.
move _ (Quit) = Left QuitError

-- Command-line stuff

-- | Exit and indicate success.
exit :: IO ()
exit = exitSuccess

-- | Exit and indicate failure.
die :: IO ()
die = exitWith $ ExitFailure 1

-- | Usage header, printed when improper command-line arguments are given.
header :: String
header = "Usage: justsudoku [-hgfV] [file]"

-- | Version number of game.
versionNum :: String
versionNum = "0.8.1.0"

-- | Full version string, including version numbert.
version :: String
version = "justsudoku version " ++ versionNum

-- | Do nothing and return an IO ().
continue :: IO ()
continue = return ()

-- | Parse command-line arguments
commandParse :: ([Flag], [String], [String]) -> IO ([Flag], [String])

-- Processing arguments if there are no errors.
commandParse (args, fs, [])
    | Help `elem` args    = do
        hPutStrLn stderr (usageInfo header options)
        exitSuccess

    | Version `elem` args = do
        hPutStrLn stderr version
        exitSuccess

    | otherwise           = do
        let files = if null fs then ["-"] else fs
        return (nub args, files)


-- Failing out
commandParse (_, _, errs)   = do
    hPutStrLn stderr (concat errs ++ usageInfo header options)
    exitWith (ExitFailure 1)

-- | Main function, runs application.
main :: IO ()
main = do

    -- Command line arguments.
    arguments <- getArgs

    let argTriple = getOpt Permute options arguments

    (args, _) <- commandParse argTriple

    -- Read from file.
    contents <- readFile "gamefiles/easy1.sfile"
    let board = parseStringToBoard contents

    -- Process arguments.
    do

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
