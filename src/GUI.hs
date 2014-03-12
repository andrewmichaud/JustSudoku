-- Andrew Michaud
-- 03/09/14
-- Stuff for GUI for Sudoku project

module GUI ( GUI
           , initSudokuGUI
           , mainWin
           ) where

import Graphics.UI.Gtk

-- Importing utilities.
import Util.Other

-- GUI datatype
data GUI = GUI { mainWin :: Window
               , mainBox :: VBox
               , tableBox :: HBox
               , table :: Table
               , menuBar :: MenuBar
               --, board :: Board
               , checkItem :: MenuItem
               , solveItem :: MenuItem
               , mainMenuItem :: MenuItem
             
               }

-- Initialize GUI type
initSudokuGUI :: IO GUI
initSudokuGUI = do
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
                  , checkItem    = cItem
                  , solveItem    = sItem
                  , mainMenuItem = mmItem
                  }
    
    bindGUI gui

-- bind items to GUI
bindGUI :: GUI -> IO GUI
bindGUI gui = do    
    -- Add items to menu.
    menuShellAppend (menuBar gui) (mainMenuItem gui)
    menuShellAppend (menuBar gui) (checkItem gui)
    menuShellAppend (menuBar gui) (solveItem gui)

    -- Grid fields.
    entryGrid <- getIOGrid 9 entryNew

    -- Apply some functions to the entries.
    _ <- addValidateFunction entryGrid 
    _ <- applyEntriesOneArg entryGrid entrySetMaxLength 1
    _ <- applyEntriesOneArg entryGrid entrySetWidthChars 4 

    -- Pack entries.
    _ <- packAllEntries (table gui) entryGrid colCoords rowCoords

    -- Set window parameters.
    set (mainWin gui) [ windowDefaultWidth   := 200
                      , windowDefaultHeight  := 200
                      , windowTitle          := "Sudoku Linux"
                      , containerChild       := (mainBox gui)
                      , containerBorderWidth := 10]
    
    -- Pack some other stuff.           
    boxPackStart (mainBox gui) (menuBar gui) PackNatural 0
    boxPackStart (mainBox gui) (tableBox gui) PackNatural 0
    boxPackStart (tableBox gui) (table gui) PackRepel 0

    return gui

-- Helper functions. Not exported.

-- Pack an entry given the entry, a table to pack in, and coordinates.
packEntry :: (TableClass self) => self -> Entry -> (Int, Int) -> (Int, Int) -> IO ()
packEntry t entry (colT, colB) (rowL, rowR) =
    tableAttach t entry colT colB rowL rowR [Shrink] [Shrink] 1 1

-- Pack a 1D list of widgets into a table with the provided indices.
packEntryList :: (TableClass self) => self -> [Entry] -> [(Int, Int)] -> [(Int, Int)] -> IO [()]
packEntryList t entryArray cols rows =
    sequence $ zipWith3 (packEntry t) entryArray cols rows

-- Pack a 2D list of entries with the provided indices.
packAllEntries :: (TableClass self) => self -> [[Entry]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> IO [[()]]
packAllEntries t entryArray cols rows =
    sequence $ zipWith3 (packEntryList t) entryArray cols rows

-- Checks whether an entry is between 1 and 9 inclusive and
-- discards it otherwise.
validateEntry :: Entry -> IO ()
validateEntry e = do
    text <- entryGetText e
    entrySetText e $ newval text
    where newval s
            | s == "" || s == " " = ""
            | otherwise           = maybe "1" show (sToIntRange s [1..9])

-- Generate one row of Sudoku coords to be used for either columns or rows.
sudokuCoords :: [(Int, Int)]
sudokuCoords = filter2
    where start  = [(x, x + 1) | x <- [0..8]] :: [(Int, Int)]
          filter1 = map (incrementTupleIf (>2) (>2)) start
          filter2 = map (incrementTupleIf (>6) (>6)) filter1

-- Generate column coordinates for a 9x9 Sudoku grid. 
colCoords :: [[(Int, Int)]]
colCoords = replicate 9 sudokuCoords :: [[(Int, Int)]]

-- Generate row coordinates for a 9x9 Sudoku grid.
rowCoords :: [[(Int, Int)]]
rowCoords = map (replicate 9) sudokuCoords :: [[(Int, Int)]] 

-- Add validate function to all entries.
addValidateFunction :: [[Entry]] -> IO [[ConnectId Entry]]
addValidateFunction = mapM addOneRow
    where addOneRow = mapM (\entry -> onEditableChanged entry (validateEntry entry))

-- Apply a function that takes an Entry and one argument to all entries.
-- Takes a list of lists of entries, an argument, and the function.
applyEntriesOneArg :: [[Entry]] -> (Entry -> a -> IO b) -> a -> IO [[b]]
applyEntriesOneArg entryArray func arg = mapM applyOneRow entryArray
    where applyOneRow = mapM (`func` arg)

