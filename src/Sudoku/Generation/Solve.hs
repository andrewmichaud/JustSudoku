-- 03/26/14

{-|

Module      : Solve
Description : Methods for solving Sudoku boards.
Copyright   : (c) Andrew Michaud, 2014
License     : Apache 2.0
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module contains methods for solving Sudoku boards.
-}

module Sudoku.Generation.Solve ( 
-- * Classes

-- * Methods

) where

import Data.Maybe
import Data.List
import System.Random

import Sudoku.Data.Board
import Util.Other

import Debug.Trace

data SolverData = SolverData { sBoard    :: SudokuBoard
                             , emptyLocs :: [Location] 
                             , lastLocs  :: [Location]
                             , badValues :: [[[Square]]]
                             , posValues :: [[[Square]]] 
                             } deriving (Show)

initSolver :: SolverData
initSolver = newData
    where newData   = SolverData { sBoard    = emptyBoard
                                 , emptyLocs = empties
                                 , lastLocs  = []
                                 , badValues = bads :: [[[Square]]]
                                 , posValues = possibles
                                 } 
          empties   = map tupleToLocation [(r, c) | r <- [0..8], c <- [0..8]]
          bads      = replicate 9 (replicate 9 [])
          possibles = replicate 9 (replicate 9 (map (fromJust . intToSquare) [1..9]))

-- | Given a grid with any number of empty squares, solve it.
solve :: SolverData -> SolverData
solve sdata

    -- Board's full, we're done.
    | null $ emptyLocs sdata = sdata

    -- More work to do.
    | otherwise = newdata

    -- Choose a random empty location.
    where randLoc   = head $ emptyLocs sdata
          randTup   = locToTuple randLoc

          -- Get possible values for this loc.
          possibles = getElem (posValues sdata) randTup
          bads      = getElem (badValues sdata) randTup
          options   = filter (`notElem` bads) possibles
          
          -- Choose one.
          randVal   = head options
          
          -- We're stuck, backtrack.
          newdata   = if null options
            then solve $ backtrackData sdata

            -- This is working, update data and recurse.
            else solve newData
                
                -- Create new data.
                where tryBoard = setBoardValue (sBoard sdata) randLoc randVal
                      
                      -- Is the board still valid?
                      newData = if null $ checkBoard tryBoard
                        
                        -- 'Tis valid, we're fine.
                        then SolverData { sBoard    = tryBoard
                                        , emptyLocs = tail $ emptyLocs sdata
                                        , lastLocs  = trace ("b size: " ++ show ( length $ randLoc : lastLocs sdata)) (randLoc : lastLocs sdata)
                                        , badValues = badValues sdata
                                        , posValues = thenPos
                                        }

                        -- Didn't work, remove that possiblity.
                        else SolverData { sBoard    = sBoard sdata
                                        , emptyLocs = emptyLocs sdata
                                        , lastLocs  = trace ("c size: " ++ show ( length $ lastLocs sdata)) (lastLocs sdata)
                                        , badValues = badValues sdata
                                        , posValues = elsePos
                                        }

                            -- Update possible values.
                            where oldPos     = posValues sdata
                                  thenPos    = updateLocs oldPos randTup (filter (/= randVal))
                                  
                                  -- Else case.
                                  oldPosList = getElem oldPos randTup
                                  elsePos    = updateLocs oldPos randTup (filter (/= randVal))
              
-- | Generate data for the backtrack case - we've messed up and need to backtrack.          
backtrackData :: SolverData -> SolverData
backtrackData sdata = SolverData { sBoard    = newBoard
                                 , emptyLocs = newEmpty
                                 , lastLocs  = trace ("a size: " ++ show (length $ tail $ lastLocs sdata)) (tail $ lastLocs sdata)
                                 , badValues = newBads
                                 , posValues = newPoss'
                                 } 

    -- Get new parameters.
    where wrongLoc   = head $ lastLocs sdata
          wrongTup   = locToTuple wrongLoc

          -- Remove value from board.
          newBoard   = eraseBoardValue (sBoard sdata) wrongLoc

          -- Update empty values.
          newEmpty   = wrongLoc : emptyLocs sdata

          -- Get badValues list to update it.
          oldBads    = badValues sdata
          oldBadList = getElem oldBads wrongTup
          newBadList = val : oldBadList
          newBads    = setElem oldBads wrongTup newBadList

          -- Get posValues list to update it.
          oldPoss    = posValues sdata
          oldPosList = getElem oldPoss wrongTup
          newPosList = filter (/= val) oldPosList
          newPoss    = setElem oldPoss wrongTup newPosList

          -- Update relevant locations
          newPoss'   = updateLocs newPoss wrongTup (val:)

          -- Gonna need this.
          val        = getBoardValue (sBoard sdata) wrongLoc

-- | Updates the possible values for all locations connected to a location -
-- all locations in the same row, column, or subgrid as the provided location.
updateLocs :: [[[Square]]] -> (Int, Int) -> ([Square] -> [Square]) -> [[[Square]]]
updateLocs oldVals loc newFunc = newPos

    -- All locations we need to update.
    where allLocs  = (getColLocs loc ++ getRowLocs loc) `union` getSubgridLocs loc
          
          -- Get all position lists and update them.
          posLists = map (newFunc . getElem oldVals) allLocs
          allData  = zip allLocs posLists

          -- Use foldl to get the new square grid.
          newPos   = foldl setElemTuple oldVals allData 

-- | Given a tuple, get all tuples in the same column as that tuple.
getColLocs :: (Int, Int) -> [(Int, Int)]
getColLocs (r, c) = [(x, c) | x <- [0..8], x /= r]

-- | Given a tuple, get all tuples in the same row as that tuple.
getRowLocs :: (Int, Int) -> [(Int, Int)]
getRowLocs (r, c) = [(r, x) | x <- [0..8], x /= c]

-- | Given a tuple, get all tuples in the same subgrid as that tuple.
getSubgridLocs :: (Int, Int) -> [(Int, Int)]
getSubgridLocs (r, c) = filter (/= (r, c)) alllocs
    where mr      = (r `quot` 3) * 3
          mc      = (c `quot` 3) * 3
          alllocs = [(rx, cx) | rx <- [mr, mr + 1, mr + 2], cx <- [mc, mc + 1, mc + 2]]

