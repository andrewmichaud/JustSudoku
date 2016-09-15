{-|
Module      : Main
Description : Handles user input and controlling everything else.
Copyright   : (c) Andrew Michaud, 2015
License     : BSD3
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module provides several things. First, it provides a control loop for receiving and
interpreting player input. This loop takes input, attempts to parse it (using Parse), gets the Move
(using Move) the player intended, applies it, and either loops with the new state of the board
after the move (if the move was valid), or loops with the old state of the board (if there was an
error). It also shows the board on each iteration and kills the program if a QuitError comes up (if
the player asked to quit).

There are also functions for each possible move, to actually apply the player's moves. If the moves
fail for any reason, these functions return errors instead of valid new boards.

The rest of the module is code to handle command line arguments, some strings holding info about
the program, and the actual Main function. The main function parses arguments, prints some stuff
out, and kickstarts the control loop.
-}

module Main (

-- * Control
  repl
, move

-- * Command-Line Processing
, commandParse

-- * Main
, main

) where

import qualified Data.List as List        (nub)
import qualified Data.Maybe as M          (fromJust, isNothing)
import qualified Data.Set as Set          (null)
import qualified Data.Version as V        (showVersion)
import Paths_JustSudoku                   (version, getDataFileName)
import System.Console.GetOpt              (getOpt, usageInfo, ArgOrder(..))
import qualified System.Environment as SE
import qualified System.Exit as SEX       (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO
import Text.Read

import Sudoku.Control.Flag
import Sudoku.Control.Move
import Sudoku.Control.Parse
import Sudoku.Data.Board

-- Styled after a friend's code.
-- | Control loop for Sudoku game. Receive user input, process it (handling errors) and repeat.
repl :: (SudokuBoard -> Move -> Either MoveError SudokuBoard) ->
        (String -> Maybe Move) ->
        SudokuBoard ->
        IO ()
repl f parse initial = do

    -- Grab input line.
    line <- getLine

    -- Handle failing to parse and succeeding (maybe) parsing.
    maybe failAction succeedAction (parse line) where

        -- If we fail to parse, say so and recurse with init.
        failAction = do
            putStrLn "Unable to parse move. Try again.\n"
            repl f parse initial

        -- After successful parse, apply the move for a new state.
        succeedAction a = do

            -- Obtain and print newstate.
            let newstate = f initial a
            putStrLn (either show prettyPrint newstate)

            -- Check if we were told to quit.
            maybeDie newstate

            -- Print prompt.
            putStrLn "What would you like to do?\n"

            -- either has type (a -> c) -> (b -> c) -> Either a b -> c
            -- I'm using the Left value of Either as an error, and the right value as an actual
            -- value. So, here we do the following.
            -- If newstate is an error, we return init. const init is a function that takes a value
            -- and throws it away, and returns init.
            -- If newstate is not an error, we return it. id just takes newstate and returns it.
            -- The end result is that we have a new value to recurse with repl.
            repl f parse (either (const initial) id newstate)

-- | Die if we received a QuitError (by using checkError to check the error).
maybeDie :: Either MoveError SudokuBoard -> IO ()
maybeDie = either checkQuit (const (return ()))

-- | Die if passed a QuitError, otherwise do nothing.
checkQuit :: MoveError -> IO ()
checkQuit QuitError = SEX.exitSuccess
checkQuit _         = return ()

-- | Try to process a move.
move :: SudokuBoard -> Move -> Either MoveError SudokuBoard

-- Set value to board. If an error occurs, the value will not be set. If no error occurs, the
-- requested value will be set.
move board (Set row col val)
    | M.isNothing rowInt   = Left $ NaNError row
    | M.isNothing colInt   = Left $ NaNError col
    | M.isNothing valInt   = Left $ NaNError val
    | M.isNothing location = Left $ OutOfBoundsError (M.fromJust rowInt) (M.fromJust colInt)
    | M.isNothing newVal   = Left $ InvalidValueError (M.fromJust valInt)
    | otherwise            = Right newBoard
    where location = toLocation row col
          newVal   = toSquare False val
          rowInt   = readMaybe row
          colInt   = readMaybe col
          valInt   = readMaybe val
          newBoard = setBoardValue board (M.fromJust location) (M.fromJust newVal)

-- Erase requested value. If an error occurs, the value will not be erased.
move board (Erase row col)
    | M.isNothing rowInt   = Left $ NaNError row
    | M.isNothing colInt   = Left $ NaNError col
    | M.isNothing location = Left $ OutOfBoundsError (M.fromJust rowInt) (M.fromJust colInt)
    | otherwise            = Right newBoard
    where location = toLocation row col
          rowInt   = readMaybe row
          colInt   = readMaybe col
          newBoard = eraseBoardValue board (M.fromJust location)

-- Check if any squares are invalid. "Error" and show invalid squares if any exist.
-- In any case, the original board will remain.
move board Check
    | not (Set.null invalidSquares) = Left $ InvalidBoardError invalidSquares
    | otherwise                     = Right board
    where invalidSquares = checkBoard board

move board Reset = Right $ resetBoard board
move _ Quit      = Left QuitError

-- Command-line stuff

-- | Usage string, printed when improper command-line arguments are given.
usage :: String
usage = "Usage: justsudoku [-hgfV] [file]"

-- | Parse command-line arguments
commandParse :: ([Flag], [String], [String]) -> IO ([Flag], [String])

-- Processing arguments if there are no errors.
commandParse (args, fs, [])
    | Help `elem` args    = do
        hPutStrLn stderr (usageInfo usage options)
        SEX.exitSuccess

    | Version `elem` args = do
        hPutStrLn stderr vstring
        SEX.exitSuccess

    | otherwise           = do
        let files = if null fs then ["-"] else fs
        return (List.nub args, files)
    where vstring = name ++ " version " ++ V.showVersion version
          name    = "JustSudoku"

-- Failing out
commandParse (_, _, errs) = do
    hPutStrLn stderr (concat errs ++ usageInfo usage options)
    SEX.exitWith (SEX.ExitFailure 1)

-- | Main function, runs application.
main :: IO ()
main = do

    -- Command line arguments.
    arguments <- SE.getArgs
    let argTriple = getOpt Permute options arguments

    -- Process arguments.
    (args, _) <- commandParse argTriple

    do
        let board = emptyBoard

        -- Game header.
        putStrLn $ "This is JustSudoku, version " ++ V.showVersion version ++ " \n"

        -- Print original board and prompt.
        putStrLn "This is the board\n"
        putStrLn $ prettyPrint board
        putStrLn "What would you like to do?\n"

        -- Enter control loop, repeat.
        repl move parseMove board

        SEX.exitSuccess
