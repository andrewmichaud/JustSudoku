module Main where

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad(replicateM)
import Data.Maybe

-- Get count entries
getEntryRow :: Int -> IO [Entry]
getEntryRow count = do
    entry <- entryNew
    row <- replicateM (count - 1)  $ do
        entry <- entryNew
        return entry
    return $ entry : row

-- Get entry grid, size count x count
getEntryGrid :: Int -> IO [[Entry]]
getEntryGrid count = do
    entryRow <- getEntryRow count
    grid <- replicateM (count - 1) $ do
        entryRow <- getEntryRow count
        return entryRow
    return $ entryRow : grid

-- Pack an entry given the entry, a table to pack in, and coordinates.
packEntry :: (WidgetClass widget, TableClass self) => self -> widget -> (Int, Int) -> (Int, Int) -> IO ()
packEntry table entry colCoords rowCoords = do
    tableAttachDefaults table entry colT colB rowL rowR
    where
        rowL = fst rowCoords
        rowR = snd rowCoords
        colT = fst colCoords
        colB = snd colCoords

-- Pack a 1D list of widgets into a table with the provided indices.
packEntryList :: (WidgetClass widget, TableClass self) => self -> [widget] -> [(Int, Int)] -> [(Int, Int)] -> [IO ()]
packEntryList table entryArray colCoords rowCoords = do
    zipWith3 (packEntry table) entryArray colCoords rowCoords

packAllEntries :: (WidgetClass widget, TableClass self) => self -> [[widget]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> [[IO ()]]
packAllEntries table entryArray colCoords rowCoords = do
    zipWith3 (packEntryList table) entryArray colCoords rowCoords

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

-- Checks whether an entry is between 1 and 9 inclusive and
-- discards it otherwise.
validateEntry :: Entry -> IO ()
validateEntry e = do
    txt <- entryGetText e
    if length txt == 0
        then do
        entrySetText e ""
        else if isJust $ checkValue txt
            then do 
                let validated = checkValue txt
                    newtxt    = show $ fromJust validated
                entrySetText e newtxt
            else do
                entrySetText e $ show 1

colCoords :: [[(Int, Int)]]
colCoords = replicate 9 [(x, x + 1) | x <- [0..8]] :: [[(Int, Int)]]

rowCoords :: [[(Int, Int)]]
rowCoords =  map (replicate 9) [(x, x + 1) | x <- [0..8]] :: [[(Int, Int)]]

main = do
    
    -- Init GUI and window handle.
    initGUI
    window  <- windowNew
    vbox    <- vBoxNew False 10
    hbox    <- hBoxNew True 10
    table   <- tableNew 9 9 True

    -- Grid fields.
    entryGrid <- getEntryGrid 9

    onEntryActivate ((entryGrid !! 0) !! 1) (validateEntry ((entryGrid !! 0) !! 1))
    -- Pack
    entrySetMaxLength ((entryGrid !! 0) !! 1) 1
    tableAttachDefaults table ((entryGrid !! 0) !! 1) 1 2 0 1

    packAllEntries table entryGrid colCoords rowCoords

    set window [windowDefaultWidth := 200
               , windowDefaultHeight := 200
               , windowTitle := "Sudoku Linux"
               , containerChild := vbox
               , containerBorderWidth := 10]
    boxPackStart vbox hbox PackNatural 0
    boxPackStart hbox table PackGrow 0
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

