-- Andrew Michaud
-- 02/15/14
-- Contains Move type, used for the player to say what they would like to do.

module Move
( Move(..)
) where

-- Types of moves the player can make.
data Move = Set String String String | Check | Quit

