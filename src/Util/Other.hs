{-|
Module      : Util.Other
Description : Hopefully useful utility functions.
Copyright   : (c) Andrew Michaud, 2015-2019
License     : BSD3
Maintainer  : dev@drew.life
Stability   : experimental

This module houses any methods that don't quite fit in anywhere else (or are generic enough to
deserve placement in a separate module).  Currently, there are two methods for incrementing
tuples, two methods for generating lists of IO objects, and a method for converting a string
into an integer conditionally.
-}

module Util.Other (

-- * Lists
  zipLists

-- * Tuples
, addToTupleIf
, incrementTupleIf

-- * Monads
, getMonadicGrid

-- * Strings
, sToIntRange
) where

import Control.Monad(replicateM)
import Text.Read(readMaybe)

-- For lists.

-- | Helper function for check functions. Given two lists of lists, returns a list
--   of lists of pairs.
zipLists :: [[a]] -> [[b]] -> [[(a, b)]]
zipLists [] _ = []
zipLists _ [] = []
zipLists (a:as) (b:bs) = zip a b : zipLists as bs

-- For tuples.

-- | Add int to tuple of int if the first condition is true for the first element of
--   the tuple and the second is true for the second element.
addToTupleIf :: Int -> (Int -> Bool) -> (Int ->Bool) -> (Int, Int) -> (Int, Int)
addToTupleIf new fstCond sndCond (f, s) = result
    where condition = fstCond f && sndCond s
          result    = if condition then (f + new, s + new) else (f, s)

-- | Special case for adding one to tuple.
incrementTupleIf :: (Int -> Bool) -> (Int -> Bool) -> (Int, Int) -> (Int, Int)
incrementTupleIf = addToTupleIf 1

-- For Monads.

-- | Given an Int n and a monad a, return a monadic list of n lists of n a.
getMonadicGrid :: (Monad m) => Int -> m a -> m [[a]]
getMonadicGrid count func = replicateM count $ replicateM count func

-- For Strings.

-- | Given a string and a range, return either Just the string converted to an Int if it is
--   in range, or Nothing.
sToIntRange :: String -> [Int] -> Maybe Int
sToIntRange str range
    | val `elem` mRange = val
    | otherwise         = Nothing
    where val    = readMaybe str
          mRange = map Just range
