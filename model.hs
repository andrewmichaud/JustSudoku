-- Andrew Michaud
-- 02/05/15
-- Model for MVC for Linux-Sudoku

import qualified Data.Vector as V
import Data.Maybe
import qualified Data.List

data Value = Number Int deriving (Show)

-- data Board = SudokuBoard [Int] [Int] [Int] [Int] [Int] [Int] [Int] [Int] [Int]

-- Main function
main = do

    -- Get all lines
    s <- getContents


    let 
        -- Split contents into an array of lines.
        allLines = lines s

        -- Split each line into an array of words.
        listOLines = [words line | line <- allLines]


    print listOLines



process s = show s 
    -- This code loads a sudoku file into the model.
    -- TODO split this into controller.
    -- TODO don't hardcode in a file.



