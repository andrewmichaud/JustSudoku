-- Main controller/logic/program
-- Andrew Michaud
-- 2/11/14

-- For argument processing.
import System.Environment

-- For file and other IO.
import System.IO

main = do

    -- Command line arguments.
    (filename:args) <- getArgs

    -- Read from file.
    contents <- readFile filename
    putStr contents
