-- Andrew Michaud
-- 03/09/14
-- View for Sudoku project.

module View where

import Graphics.UI.Gtk
import GUI

-- Initialize Sudoku view
initSudokuView :: IO Window 
initSudokuView = do
    
    -- Init GUI and window handle.
    -- discarding string that initGUI returns for some reason.
    gui       <- initSudokuGUI

    return (mainWin gui)

-- Run GUI window
runSudokuWindow :: WindowClass window => window -> IO ()
runSudokuWindow window = do 
    _ <- onDestroy window mainQuit
    widgetShowAll window
    mainGUI

