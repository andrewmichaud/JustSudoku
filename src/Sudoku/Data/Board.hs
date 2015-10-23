{-# OPTIONS_HADDOCK not-home, show-extensions #-}
{-|

Module      : Board
Description : Methods and datatypes for creating and manipulating Sudoku Boards.
Copyright   : (c) Andrew Michaud, 2015
License     : BSD3
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module exports the methods to create a SudokuBoard, the types used to construct one, and
all of the methods used to interact with one once it's created.

-}

module Sudoku.Data.Board (
-- * Classes
  SqVal
, Square
, SudokuBoard
, Location

-- * Methods
, isEmpty

-- * Location, Square Generation
, toLocation
, tupleToLocation
, toSquare

-- * Board Generation
, emptyBoard
, parseStringToBoard

-- * Printing
, prettyPrint

-- * Board Modification
, getBoardValue
, setBoardValue
, eraseBoardValue
, resetBoard
, checkBoard
) where

import Sudoku.Data.Board.Internal
