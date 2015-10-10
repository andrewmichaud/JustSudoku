module Sudoku.Control.Move.Internal where

-- For custom errors.
import Control.Monad.Except

import Sudoku.Data.Board

-- | Types of moves the player can make.
data Move = Set String String String | -- ^ Set square, provides coordinates and new value.
            Check                    | -- ^ Check board.
            Erase String String      | -- ^ Erase square, provides coordinates.
            Reset                    | -- ^ Reset board.
            Quit                       -- ^ Quit game.
            deriving (Show, Eq)

-- | Kinds of move errors that can occur.
data MoveError = NaNError String              | -- ^ Provided value is not a number.
                 OutOfBoundsError Int Int     | -- ^ Provided location is out of bounds.
                 InvalidValueError Int        | -- ^ Provided value is invalid.
                 InvalidBoardError [Location] | -- ^ Returned locations are invalid.
                 QuitError                    | -- ^ Asked to quit game.
                 OtherError String              -- ^ Other error not described above.

-- | What each error shows when it occurs.
instance Show MoveError where
    show (NaNError val)              = "Value " ++ val ++ " is not a number."
    show (OutOfBoundsError row col)  = "Square (" ++ show row ++ ", " ++ show col
                                                  ++ ") is out of bounds."
    show (InvalidValueError val)     = "Value " ++ show val ++ " is invalid."
    show (InvalidBoardError squares) = "Board is invalid. Invalid squares: " ++ show squares
    show (OtherError string)         = "General error: " ++ string
    show QuitError                   = "Asked or required to quit!"
