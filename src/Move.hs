-- Andrew Michaud
-- 02/15/14
-- Contains Move type, used for the player to say what they would like to do.

module Move
( Move(..)
, MoveError(..)
) where

-- For custom errors.
import Control.Monad.Error

import Board

-- | Types of moves the player can make.
data Move = Set String String String | -- ^ Set square, provides coordinates and new value.
            Check                    | -- ^ Check board.
            Erase String String      | -- ^ Erase square, provides coordinates.
            Reset                    | -- ^ Reset board.
            Quit                       -- ^ Quit game.

-- | Kinds of move errors that can occur.
data MoveError = NaNError String              | -- ^ Provided value is not a number.
                 OutOfBoundsError Int Int     | -- ^ Provided location is out of bounds.
                 InvalidValueError Int        | -- ^ Provided value is invalid.
                 InvalidBoardError [Location] | -- ^ Returned locations are invalid.
                 QuitError                    | -- ^ Asked to quit game.
                 OtherError String              -- ^ Other error not described above.

-- | Error for a MoveError.  This shouldn't happen.
instance Error MoveError where
    noMsg  = OtherError "Something has gone horribly wrong."
    strMsg = OtherError

-- | What each error shows when it occurs.
instance Show MoveError where
    show (NaNError value)            = "Value " ++ value ++ " is not a number."
    show (OutOfBoundsError row col)  = "Square (" ++ show row ++ ", " ++ show col 
                                                 ++ " is out of bounds."
    show (InvalidValueError value)   = "Value " ++ show value ++ " is invalid."
    show (InvalidBoardError squares) = "Board is invalid. Invalid squares: " ++ show squares
    show (OtherError string)         = "General error: " ++ string
    show QuitError                   = "Asked or required to quit!"

