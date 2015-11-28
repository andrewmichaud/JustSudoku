{-|

Module      : Control.Parse
Description : Methods for parsing moves and for creating locations and squares.
Copyright   : (c) Andrew Michaud, 2015
License     : BSD3
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module includes a parseMove method, which takes a string and returns a Move (of those defined
in the Move module) that matches. If no move matches, Nothing is returned.  These methods need to
parse raw user input, so they're somewhat strict on what they accept as valid input.

-}

module Sudoku.Control.Parse where

import Data.Char  (toLower)
import Data.Maybe (isNothing)

import Sudoku.Control.Move
import Sudoku.Data.Board

-- | Parse a move into Check, Erase, Quit, Reset, or Set.
parseMove :: String -> Maybe Move
parseMove "" = Nothing

parseMove command
    | map toLower (head split) == "check" = Just Check
    | map toLower (head split) == "erase" = eraseMove (tail split)
    | map toLower (head split) == "quit"  = Just Quit
    | map toLower (head split) == "reset" = Just Reset
    | map toLower (head split) == "set"   = setMove (tail split)
    where split = words command

-- Simple versions of commands. Not user-friendly, but they're what I originally came up with.
-- Set value on board.
parseMove ('s':r:c:v)
    | isNothing val = Nothing
    | isNothing loc = Nothing
    | otherwise     = Just $ Set [r] [c] v
    where loc = toLocation [r] [c]
          val = toSquare False v

-- Erase value from board.
parseMove ('e':r:c)
    | isNothing loc = Nothing
    | otherwise     = Just $ Erase [r] c
    where loc = toLocation [r] c

-- Check board, quit game, reset board.
parseMove "c" = Just Check
parseMove "q" = Just Quit
parseMove "r" = Just Reset
parseMove _   = Nothing

-- | Set a value on the board. Takes in three strings representing, respectively, row and column,
--   and value.
setMove :: [String] -> Maybe Move
setMove arguments
    | length arguments < 3 = Nothing
    | isNothing loc        = Nothing
    | isNothing v          = Nothing
    | otherwise            = Just $ Set row col val
    where row = head arguments
          col = arguments !! 1
          val = arguments !! 2
          loc = toLocation row col
          v   = toSquare False val

-- | Erase a value on the board. Takes in two strings representing, respectively, row and column.
eraseMove :: [String] -> Maybe Move
eraseMove arguments
    | length arguments < 2 = Nothing
    | isNothing loc        = Nothing
    | otherwise            = Just $ Erase row col
    where row = head arguments
          col = arguments !! 1
          loc = toLocation row col
