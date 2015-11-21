{-|

Module      : Control.Move
Description : Defines valid Moves and MoveErrors for Sudoku application.
Copyright   : (c) Andrew Michaud, 2015
License     : BSD3
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module describes the Moves a player is allowed to make on the Sudoku board. It also describes
the possible MoveErrors that can result if the player's move is improper. These two types are used
by Parse and the controller to interpret player commands and to inform the player of the results of
their moves.

-}

module Sudoku.Control.Move where

import Control.Monad.Except -- For custom errors.
import Data.Set

import Sudoku.Data.Board

-- | Types of moves the player can make.
data Move = Check                    | -- ^ Check board.
            Erase String String      | -- ^ Erase square, provides coordinates.
            Quit                     | -- ^ Quit game.
            Reset                    | -- ^ Reset board.
            Set String String String   -- ^ Set square, provides coordinates and new value.
            deriving (Show, Eq)

-- | Kinds of move errors that can occur.
data MoveError = InvalidBoardError (Set Location) | -- ^ Returned locations are invalid.
                 InvalidValueError Int            | -- ^ Provided value is invalid.
                 NaNError String                  | -- ^ Provided value is not a number.
                 OtherError String                | -- ^ Other error not described above.
                 OutOfBoundsError Int Int         | -- ^ Provided location is out of bounds.
                 QuitError                          -- ^ Asked to quit game.

-- | What each error shows when it occurs.
instance Show MoveError where
    show (InvalidBoardError squares) = "Board is invalid. Invalid squares: " ++ show squares
    show (InvalidValueError val)     = "Value " ++ show val ++ " is invalid."
    show (NaNError val)              = "Value " ++ val ++ " is not a number."
    show (OtherError string)         = "General error: " ++ string
    show (OutOfBoundsError row col)  = "Square (" ++ show row ++ ", " ++ show col
                                                  ++ ") is out of bounds."
    show QuitError                   = "Asked or required to quit!"
