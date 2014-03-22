-- 03/09/14

{-|

Module      : View
Description : Wraps GUI in a module to interface with controller.
Copyright   : (c) Andrew Michaud, 2014
License     : Apache 2.0
Maintainer  : andrewjmichaud@gmail.com
Stability   : experimental

This module exists because I'm trying to follow the MVC paradigm with this application.  This 
module wraps all of the GUI stuff together, so the controller can simply call these two methods
to start the GUI if the user asks for it.  Currently, there seems to be no way to control the GUI
from the controller once it has started, so this approach might need to be changed.

-}

module View where

import Graphics.UI.Gtk
import GUI

-- | Initialize Sudoku view.
initSudokuView :: IO Window 
initSudokuView = do
    
    -- Init GUI and window handle.
    -- discarding string that initGUI returns for some reason.
    gui       <- initSudokuGUI

    return (mainWin gui)

-- | Run GUI window.
runSudokuWindow :: WindowClass window => window -> IO ()
runSudokuWindow window = do 
    _ <- onDestroy window mainQuit
    widgetShowAll window
    mainGUI

