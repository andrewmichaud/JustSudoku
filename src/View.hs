
-- Andrew Michaud
-- 03/09/14
-- View for Sudoku project.

module View where

import Graphics.UI.Gtk
--import Control.Monad.Trans(liftIO)
import Control.Monad(replicateM)
import Data.Maybe

-- Get count entries
getEntryRow :: Int -> IO [Entry]
getEntryRow count = do
    entry <- entryNew
    row <- replicateM (count - 1)  $ do
        nextEntry <- entryNew
        return nextEntry
    return $ entry : row

-- Get entry grid, size count x count
getEntryGrid :: Int -> IO [[Entry]]
getEntryGrid count = do
    entryRow <- getEntryRow count
    grid <- replicateM (count - 1) $ do
        nextEntryRow <- getEntryRow count
        return nextEntryRow
    return $ entryRow : grid

-- Pack an entry given the entry, a table to pack in, and coordinates.
packEntry :: (TableClass self) => self -> Entry -> (Int, Int) -> (Int, Int) -> IO ()
packEntry table entry (colT, colB) (rowL, rowR) = do
    tableAttach table entry colT colB rowL rowR [Shrink] [Shrink] 1 1

-- Pack a 1D list of widgets into a table with the provided indices.
packEntryList :: (TableClass self) => self -> [Entry] -> [(Int, Int)] -> [(Int, Int)] -> IO [()]
packEntryList table entryArray cols rows = do
    sequence $ zipWith3 (packEntry table) entryArray cols rows

-- Pack a 2D list of entries with the provided indices.
packAllEntries :: (TableClass self) => self -> [[Entry]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> IO [[()]]
packAllEntries table entryArray cols rows = do
    sequence $ zipWith3 (packEntryList table) entryArray cols rows

-- Convert from int to string, discarding out of range.
checkValue :: String -> Maybe Int
checkValue "1" = Just 1
checkValue "2" = Just 2
checkValue "3" = Just 3
checkValue "4" = Just 4
checkValue "5" = Just 5
checkValue "6" = Just 6
checkValue "7" = Just 7
checkValue "8" = Just 8
checkValue "9" = Just 9
checkValue _   = Nothing

-- Adds an int to a tuple of ints.
addToTuple :: Int -> (Int, Int) -> (Int, Int)
addToTuple num oldTuple = ((fst oldTuple) + num, (snd oldTuple) + num)

-- Add int to tuple of int if two conditions are true.
addToTupleIf :: Int -> (Int -> Bool) -> (Int ->Bool) -> (Int, Int) -> (Int, Int)
addToTupleIf new fstCond sndCond (f, s) = result
    where condition = fstCond f && sndCond s
          result    = if condition then (f + new, s + new) else (f, s)

-- Special case for adding one to tuple.
incrementTupleIf :: (Int -> Bool) -> (Int -> Bool) -> (Int, Int) -> (Int, Int)
incrementTupleIf fstCond sndCond oldTuple = addToTupleIf 1 fstCond sndCond oldTuple

-- Checks whether an entry is between 1 and 9 inclusive and
-- discards it otherwise.
validateEntry :: Entry -> IO ()
validateEntry e = do
    text <- entryGetText e
    entrySetText e $ newval text
    where newval s
            | length s == 0         = ""
            | s == " "              = ""
            | isJust $ checkValue s = show $ fromJust $ checkValue s
            | otherwise             = "1" -- temporary until I can store this somewhere.

-- Generate column coordinates for a 9x9 Sudoku grid. 
colCoords :: [[(Int, Int)]]
colCoords = final
    where oneRow  = [(x, x + 1) | x <- [0..8]] :: [(Int, Int)]
          filter1 = map (incrementTupleIf (>2) (>2)) oneRow
          filter2 = map (incrementTupleIf (>6) (>6)) filter1
          final   = replicate 9 filter2 :: [[(Int, Int)]]

-- Generate row coordinates for a 9x9 Sudoku grid.
rowCoords :: [[(Int, Int)]]
rowCoords = final
    where oneRow  = [(x, x + 1) | x <- [0..8]] :: [(Int, Int)]
          filter1 = map (incrementTupleIf (>2) (>2)) oneRow
          filter2 = map (incrementTupleIf (>6) (>6)) filter1
          final   = map (replicate 9) filter2 :: [[(Int, Int)]] 

-- Add validate function to all entries.
addValidateFunction :: [[Entry]] -> IO [[ConnectId Entry]]
addValidateFunction entryArray = do
    sequence $ map (\row -> validateOne row) entryArray
    where
        validateOne row = sequence $ map (\entry -> onEditableChanged entry (validateEntry entry)) row

-- Set size request of all entries.
setAllEntriesWidth :: Int -> [[Entry]] -> IO [[()]]
setAllEntriesWidth size entryArray = do
    sequence $ map (\row -> setOne row) entryArray
    where
        setOne row = sequence $ map (\entry -> entrySetWidthChars entry size) row

-- Set max chars for all entries.
setAllEntriesMaxChars :: Int -> [[Entry]] -> IO [[()]]
setAllEntriesMaxChars size entryArray = do
    sequence $ map (\row -> setOne row) entryArray
    where
        setOne row = sequence $ map (\entry -> entrySetMaxLength entry size) row


-- Initialize Sudoku view
initSudokuView :: IO Window 
initSudokuView = do
    
    -- Init GUI and window handle.
    -- discarding string that initGUI returns for some reason.
    _        <- initGUI
    window   <- windowNew

    -- Init other items.
    mainBox  <- vBoxNew False 10
    tableBox <- hBoxNew False 10
    table    <- tableNew 10 10 True
    menuBar  <- menuBarNew

    -- Grid fields.
    entryGrid <- getEntryGrid 9
   
    -- Menu bar items. 
    checkItem    <- menuItemNewWithMnemonic "_Check"
    solveItem    <- menuItemNewWithMnemonic "_Solve"
    mainMenuItem <- menuItemNewWithMnemonic "_Main Menu"

    -- Add items to menu
    menuShellAppend menuBar mainMenuItem
    menuShellAppend menuBar checkItem
    menuShellAppend menuBar solveItem

    -- Apply some functions to the entries.
    _ <- addValidateFunction entryGrid
    _ <- setAllEntriesWidth 4 entryGrid
    _ <-setAllEntriesMaxChars 1 entryGrid

    -- Pack entries.
    _ <- packAllEntries table entryGrid colCoords rowCoords

    -- Set window parameters.
    set window [windowDefaultWidth := 200
               , windowDefaultHeight := 200
               , windowTitle := "Sudoku Linux"
               , containerChild := mainBox
               , containerBorderWidth := 10]
    
    -- Pack some other stuff.           
    boxPackStart mainBox menuBar PackNatural 0
    boxPackStart mainBox tableBox PackNatural 0
    boxPackStart tableBox table PackRepel 0

    return window

-- Run GUI window
runSudokuWindow :: WindowClass window => window -> IO ()
runSudokuWindow window = do 
    _ <- onDestroy window mainQuit
    widgetShowAll window
    mainGUI

