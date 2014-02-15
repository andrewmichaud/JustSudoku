-- Main controller/logic/program
-- Andrew Michaud
-- 2/11/14

module Main where

-- For argument processing.
import System.Environment

-- For file and other IO.
import System.IO

import Data.Maybe

import SudokuBoard

-- Styled (with permission) after a friend's code.
repl :: (Show a, Show c) => (a -> b -> Either a c) -> (String -> Maybe b) -> a -> IO ()
repl f parse init = do
    
    -- Grab input line.
    line <- getLine
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
parseMove :: String -> Maybe (Location, Square)
-- Take a move in the form "rcs", where "r" indicates the row index, "c" the column index,
-- and "s" the value to set the square at those indices.
parseMove (row:col:sq)
    | loc    == Nothing = Nothing
    | square == Nothing = Nothing
    | otherwise         = Just (fromJust loc, fromJust square)
    where loc       = parseLocation [row] [col]
          square    = parseSquare sq

-- Turn an Int into a square (or nothing).
-- 0 is turned into Empty.
parseSquare :: String -> Maybe Square
parseSquare intString = toSquare intString

-- Turn two ints into a Location
parseLocation :: String -> String -> Maybe Location
parseLocation rowString colString = toLocation rowString colString

main = do

    -- Command line arguments.
    arguments <- getArgs

    -- Read from file.
    --contents <- readFile filename
    

    let board   = emptyBoard
        pp      = prettyPrint board

    putStrLn pp

