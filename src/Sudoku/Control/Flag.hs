{-|

Module      : Control.Flag
Description : Defines flags and option descriptions.
Copyright   : (c) Andrew Michaud, 2015-2019
License     : BSD3
Maintainer  : dev@drew.life
Stability   : experimental

This module describes the flags permitted for the Sudoku application.  The flags exist as a data
type, and descriptions and long and short option flags are provided as well.

-}

module Sudoku.Control.Flag (

-- * Classes
  Flag(..)

-- * Option Descriptions
, options
) where

import System.Console.GetOpt

-- | Datatype for command-line flags.
data Flag
    = Graphical   -- ^ Enable graphical mode - --graphical, -g
    | Help        -- ^ Show help - --help, -h
    | Version     -- ^ Show version information - version, -V
    | File String -- ^ Provide a file to load from - --file, -f
    deriving (Eq, Ord, Show)

-- | Describe all allowed command-line options.
options :: [OptDescr Flag]
options = [ Option "g" ["graphical"] (NoArg Graphical)
            "Start the graphical interface."
          , Option "f" ["file"]      (ReqArg File "File")
            "Provide a file to load a Sudoku board from."
          , Option "V" ["version"]   (NoArg Version)
            "Print out version information."
          , Option "h" ["help"]      (NoArg Help)
            "Print this help message."
          ]
