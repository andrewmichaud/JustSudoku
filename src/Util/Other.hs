-- Andrew Michaud
-- 03/09/14
-- Utility functions.

module Util.Other where

import Control.Monad(replicateM)
import Text.Read(readMaybe)

-- For tuples

-- | Add int to tuple of int if the first condition is true for the first element of
--   the tuple and the second is true for the second element.
addToTupleIf :: Int -> (Int -> Bool) -> (Int ->Bool) -> (Int, Int) -> (Int, Int)
addToTupleIf new fstCond sndCond (f, s) = result
    where condition = fstCond f && sndCond s
          result    = if condition then (f + new, s + new) else (f, s)

-- | Special case for adding one to tuple.
incrementTupleIf :: (Int -> Bool) -> (Int -> Bool) -> (Int, Int) -> (Int, Int)
incrementTupleIf = addToTupleIf 1


-- For IO

-- | Given an Int n and a function returning an IO a, return an IO list of n a.
getIORow :: Int -> IO a -> IO [a]
getIORow count func = do
    entry <- func
    row   <- replicateM (count - 1) func
    return $ entry : row

-- | Given an Int n and a function returning an IO a, return an IO list of lists of n a.
getIOGrid :: Int -> IO a -> IO [[a]]
getIOGrid count func = do
    entryRow <- getIORow count func
    grid     <- replicateM (count - 1) $ getIORow count func 
    return $ entryRow : grid


-- For Strings.

-- | Given a string and a range, return either Just the string converted to an Int if it is 
--   in range, or Nothing.
sToIntRange :: String -> [Int] -> Maybe Int
sToIntRange str range
    | val `elem` mRange = val
    | otherwise         = Nothing
    where val = readMaybe str
          mRange = map Just range

