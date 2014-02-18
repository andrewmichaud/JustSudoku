module Main where

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main = do
    
    -- Init GUI and window handle.
    initGUI
    window  <- windowNew
    vbox    <- vBoxNew False 10
    hbox    <- hBoxNew True 10
    set window [windowDefaultWidth := 200
               , windowDefaultHeight := 200
               , windowTitle := "Sudoku Linux"
               , containerChild := vbox
               , containerBorderWidth := 10]
    onClicked button (hello button)
    boxPackStart vbox hbox PackNatural 0
    boxPackStart hbox button PackGrow 0
    boxPackStart hbox button PackGrow 0
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
