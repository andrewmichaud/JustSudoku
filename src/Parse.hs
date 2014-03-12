-- Andrew Michaud
-- 02/15/14
-- Contains methods for parsing moves for a Sudoku game.

module Parse where

import Move
import Board
import Data.Maybe

-- Parse a move
-- Also styled after a friend's code.
parseMove :: String -> Maybe Move

-- Set value at row and column to value.
parseMove ('s':r:c:v)
    | isNothing loc = Nothing
    | isNothing val = Nothing
    | otherwise     = Just $ Set [r] [c] v
    where
        loc = parseLocation [r] [c]
        val = parseSquare v

-- Check if board is valid.
parseMove "c" = Just Check

-- Quit.
parseMove "q" = Just Quit

-- Reset board.
parseMove "r" = Just Reset

-- Erase value in board.
parseMove ('e':r:c)
    | isNothing loc = Nothing
    | otherwise     = Just $ Erase [r] c
    where
        loc = parseLocation [r] c

-- No other moves are valid.
parseMove _ = Nothing

-- Turn an Int into a square (or nothing).
-- 0 is turned into Empty.
parseSquare :: String -> Maybe Square
parseSquare = toSquare

-- Turn two ints into a Location
parseLocation :: String -> String -> Maybe Location
parseLocation = toLocation

