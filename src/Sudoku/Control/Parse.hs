{-| Module      : Control.Parse
    Description : Methods for parsing moves and for creating locations and squares.
    Copyright   : (c) Andrew Michaud, 2015
    License     : BSD3
    Maintainer  : andrewjmichaud@gmail.com
    Stability   : experimental

    This module includes a parseMove method, which takes a string and returns a Move (of those
    defined in the Move module) that matches.  If no move matches, Nothing is returned.  Functions
    are also provided to turn two possibly-valid coordinates (as strings) into a Location, and to
    turn a possibly-valid string into the value of a new Square.  These methods need to parse raw
    user input, so they're rather strict on what they accept as valid input.
-}

module Sudoku.Control.Parse where

import Data.Char(toLower)
import Data.Maybe(isNothing)

import Sudoku.Control.Move
import Sudoku.Data.Board

-- Also styled after a friend's code.
-- | Parse a move
parseMove :: String -> Maybe Move
parseMove "" = Nothing

parseMove command
    | map toLower (head split) == "check" = Just Check
    | map toLower (head split) == "erase" = eraseMove (tail split)
    | map toLower (head split) == "Quit"  = Just Quit
    | map toLower (head split) == "reset" = Just Reset
    | map toLower (head split) == "set"   = setMove (tail split)
    where split = words command

-- Simple versions of commands. Not user-friendly, but they're what I originally came up with, so
-- they stay.
-- Set value on board.
parseMove ('s':r:c:v)
    | isNothing val = Nothing
    | otherwise     = Just $ Set [r] [c] v
    where val = parseSquare v

-- Erase value from board.
parseMove ('e':r:c)
    | isNothing loc = Nothing
    | otherwise     = Just $ Erase [r] c
    where loc = parseLocation [r] c

-- Check board, quit game, reset board.
parseMove "c" = Just Check
parseMove "q" = Just Quit
parseMove "r" = Just Reset
parseMove _   = Nothing

setMove :: [String] -> Maybe Move
setMove arguments
    | length arguments < 3 = Nothing
    | isNothing l          = Nothing
    | isNothing v          = Nothing
    | otherwise            = Just $ Set row col val
    where row = head arguments
          col = arguments !! 1
          val = arguments !! 2
          l   = parseLocation row col
          v   = parseSquare val

eraseMove :: [String] -> Maybe Move
eraseMove arguments
    | length arguments < 2 = Nothing
    | isNothing l          = Nothing
    | otherwise            = Just $ Erase row col
    where row = head arguments
          col = arguments !! 1
          l   = parseLocation row col

-- | Turn an Int into a square (or nothing).
--   0 is turned into Empty.
parseSquare :: String -> Maybe Square
parseSquare = toSquare False

-- | Turn two ints into a Location
parseLocation :: String -> String -> Maybe Location
parseLocation = toLocation
