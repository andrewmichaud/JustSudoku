-- 03/09/14

{-|

Module      : GUI
Description : Methods for creating a Sudoku GTK+ GUI.
Copyright   : (c) Andrew Michaud, 2014
License     : Apache 2.0
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module contains a datatype for a Sudoku application GUI, which includes everything I think
is necessary for a reasonable Sudoku application. It also includes a method for initializing
a GUI, a method for binding all of the elements together, and exports the mainWin field so
code importing this module can actually use the GUI.

-}

module Sudoku.GTK.GUI (

-- * Classes 
  GUI

-- * Constructors
, initSudokuGUI

-- * Exported Record Entries
, mainWin
) where

import Graphics.UI.Gtk
import Data.IORef

import Sudoku.Data.Board

-- Importing utilities.
import Util.Other

-- | GUI datatype for a Sudoku window.
data GUI = GUI { mainWin      :: Window      -- ^ Main window.
               , mainBox      :: VBox        -- ^ VBox holding everything.
               , tableBox     :: HBox        -- ^ HBox holding table.
               , table        :: Table       -- ^ Table holding Sudoku grid entries.
               , menuBar      :: MenuBar     -- ^ Menu bar for application.
               , board        :: SudokuBoard -- ^ Board holding game info.
               , checkItem    :: MenuItem    -- ^ Menu item to check board validity.
               , solveItem    :: MenuItem    -- ^ Menu item to submit board as solved.
               , mainMenuItem :: MenuItem    -- ^ Menu item to return to main menu.
             
               }

-- | Initialize a Sudoku GUI and setup GTK+.
initSudokuGUI :: SudokuBoard -> IO GUI
initSudokuGUI sb = do
    
    -- Do this first or GTK+ gets very sad.
    _ <- initGUI

    -- Initialize stuff.
    w      <- windowNew
    mBox   <- vBoxNew False 10
    tBox   <- hBoxNew False 10
    t      <- tableNew 10 10 True
    mBar   <- menuBarNew
    cItem  <- menuItemNewWithMnemonic "_Check"
    sItem  <- menuItemNewWithMnemonic "_Solve"
    mmItem <- menuItemNewWithMnemonic "_Main Menu"
    
    let gui = GUI { mainWin      = w
                  , mainBox      = mBox
                  , tableBox     = tBox
                  , table        = t
                  , menuBar      = mBar
                  , board        = sb
                  , checkItem    = cItem
                  , solveItem    = sItem
                  , mainMenuItem = mmItem
                  }
    
    bindGUI sb gui

-- | Bind GUI elements to the window.
bindGUI :: SudokuBoard -> GUI -> IO GUI
bindGUI sb gui = do    
    
    -- Add items to menu.
    menuShellAppend (menuBar gui) (mainMenuItem gui)
    menuShellAppend (menuBar gui) (checkItem gui)
    menuShellAppend (menuBar gui) (solveItem gui)

    -- Grid fields.
    entryGrid <- getIOGrid 9 entryNew

    -- Fill fields.
    let locs = [[tupleToLocation (r, c) | c <- [0..8]] | r <- [0..8]]
    setEntryGrid sb locs entryGrid

    -- Apply some functions to the entries.
    _ <- addValidateFunction entryGrid 
    _ <- applyEntriesOneArg entryGrid entrySetMaxLength 1
    _ <- applyEntriesOneArg entryGrid entrySetWidthChars 4 

    -- Pack entries.
    _ <- packAll (table gui) entryGrid colCoords rowCoords

    -- Set window parameters.
    set (mainWin gui) [ windowDefaultWidth   := 200
                      , windowDefaultHeight  := 200
                      , windowTitle          := "Just Sudoku"
                      , containerChild       := mainBox gui
                      , containerBorderWidth := 10]
    
    -- Pack some other stuff.           
    boxPackStart (mainBox gui) (menuBar gui) PackNatural 0
    boxPackStart (mainBox gui) (tableBox gui) PackNatural 0
    boxPackStart (tableBox gui) (table gui) PackRepel 0

    return gui

-- Helper functions. Not exported.

-- | Pack a 2D list of widgets into a table with the provided indices.
packAll :: (TableClass self, WidgetClass w) => self -> [[w]]  -> [[(Int, Int)]] ->
                                                      [[(Int, Int)]] -> IO [[()]]
packAll tab wArray cols rows =
    sequence $ zipWith3 (packList tab) wArray cols rows
    where packList wTab wArr c r      = sequence $ zipWith3 (pack wTab) wArr c r
          pack wTab w (c, cs) (r, rs) = tableAttachDefaults wTab w c cs r rs

-- | Set up one entry with value and original status.
setOne :: SudokuBoard -> Location -> Entry -> IO ()
setOne b loc entry
    | isEmpty square = return ()
    | otherwise      = do
        
        -- Fill entries and make them uneditable.
        entrySetText entry $ show square
        widgetSetSensitivity entry False
    where square    = getBoardValue b loc

-- | Set up a list of entries.
setEntryList :: SudokuBoard -> [Location] -> [Entry] -> IO ()
setEntryList b locList entryList
    | length entryList == 1 = setOne b (head locList) (head entryList)
    | otherwise             = do
        setOne b (head locList) (head entryList)
        setEntryList b (tail locList) (tail entryList)
        
-- | Set up a grid of entries.
setEntryGrid :: SudokuBoard -> [[Location]] -> [[Entry]] -> IO ()
setEntryGrid b locGrid entryGrid
    | length entryGrid == 1 = setEntryList b (head locGrid) (head entryGrid)
    | otherwise             = do
        setEntryList b (head locGrid) (head entryGrid)
        setEntryGrid b (tail locGrid) (tail entryGrid)

-- | Add validate function to all entries.
addValidateFunction :: [[Entry]] -> IO [[()]]
addValidateFunction = mapM addOneRow
    where addOneRow      = mapM oneEntry
            
          oneEntry entry = do
            
            -- GTK black magic.
            idRef  <- newIORef undefined
            textId <- onInsertText entry $ \str pos -> do
                readId <- readIORef idRef
                
                -- Get old value.
                oldVal <- editableGetChars entry 0 1
                
                signalBlock readId
                pos'   <- editableInsertText entry (check oldVal str) pos
                signalUnblock readId
                stopInsertText readId
                
                -- Return new position.
                return pos'
            writeIORef idRef textId

          -- Check that new value is a valid Sudoku value.
          check old new
            | new == " " || new == "" = ""
            | otherwise           = maybe old show $ sToIntRange new [1..9]

-- | Generate one row of Sudoku coords to be used for either columns or rows.
sudokuCoords :: [(Int, Int)]
sudokuCoords = filter2
    where start  = [(x, x + 1) | x <- [0..8]] :: [(Int, Int)]
          filter1 = map (incrementTupleIf (>2) (>2)) start
          filter2 = map (incrementTupleIf (>6) (>6)) filter1

-- | Generate column coordinates for a 9x9 Sudoku grid. 
colCoords :: [[(Int, Int)]]
colCoords = replicate 9 sudokuCoords :: [[(Int, Int)]]

-- | Generate row coordinates for a 9x9 Sudoku grid.
rowCoords :: [[(Int, Int)]]
rowCoords = map (replicate 9) sudokuCoords :: [[(Int, Int)]] 

-- | Apply a function that takes an Entry and one argument to all entries.
--   Takes a list of lists of entries, an argument, and the function.
applyEntriesOneArg :: [[Entry]] -> (Entry -> a -> IO b) -> a -> IO [[b]]
applyEntriesOneArg entryArray func arg = mapM applyOneRow entryArray
    where applyOneRow = mapM (`func` arg)

