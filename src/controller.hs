-- Main controller/logic/program
-- Andrew Michaud
-- 2/11/14

module Main where

-- For argument processing.
import System.Environment

-- For file and other IO.
import System.IO

import SudokuBoard

-- Copied (with permission) from a friend's code.
repl :: (Show a, Show c) => (a -> b -> Either a c) -> (String -> Maybe b) -> a -> IO ()
repl f parse init = do
    z <- getLine
    maybe fail succeed (parse z) where
        fail = do
            print "Parse error. Try again."
            repl f parse init
        succeed a = do
            let newstate = f init a
            print newstate
            repl f parse $ either id (const init) newstate

main = do

    -- Command line arguments.
    --(filename:args) <- getArgs

    -- Read from file.
    --contents <- readFile filename
    

    let board   = emptyBoard
        pp      = prettyPrint board

    putStrLn pp

