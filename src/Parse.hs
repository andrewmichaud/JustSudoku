-- 02/15/14

{-|

Module      : Parse
Description : Methods for parsing moves and for creating locations and squares.
Copyright   : (c) Andrew Michaud, 2014
License     : Apache 2.0
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module includes a parseMove method, which takes a string and returns a Move (of those
defined in the Move module) that matches.  If no move matches, Nothing is returned.  Functions are
also provided to turn two possibly-valid coordinates (as strings) into a Location, and to turn a 
possibly-valid string into the value of a new Square.  These methods need to parse raw user
input, so they're rather strict on what they accept as valid input.

-}

module Parse where

import Move
import Board
import Data.Maybe

-- Also styled after a friend's code.
-- | Parse a move
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

-- | Turn an Int into a square (or nothing).
--   0 is turned into Empty.
parseSquare :: String -> Maybe Square
parseSquare = toSquare

-- | Turn two ints into a Location
parseLocation :: String -> String -> Maybe Location
parseLocation = toLocation

