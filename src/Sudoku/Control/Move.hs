{-|

Module      : Control.Move
Description : Defines valid Moves and MoveErrors for Sudoku application.
Copyright   : (c) Andrew Michaud, 2015
License     : BSD3
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module describes the Moves a player is allowed to make on the Sudoku board.  It also
describes the possible MoveErrors that can result if the player's move is improper.  These two
types are used by Parse and the controller to interpret player commands and to inform the player
of the results of their moves.

-}

module Sudoku.Control.Move (

-- * Classes
  Move(..)
, MoveError
) where

import Sudoku.Control.Move.Internal
