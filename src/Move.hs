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
data Move = Set String String String | Check | Erase String String | Reset | Quit

-- | Kinds of move errors that can occur.
data MoveError = NaNError String | OutOfBoundsError Int Int | InvalidValueError Int | 
                 OtherError String | InvalidBoardError [Location] | QuitError

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

