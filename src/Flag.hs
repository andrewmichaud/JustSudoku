-- Andrew Michaud
-- 03/08/14
-- Command line flags for Sudoku game.

module Flag where

import System.Console.GetOpt

-- | Datatype for command-line flags.
data Flag
    = Graphical   -- --graphical, -g
    | Help        -- --help, -h
    | Version     -- --version, -v
    | File String -- -f <file>
    deriving (Eq, Ord, Show)

-- | Describe all allowed command-line options.
options :: [OptDescr Flag]
options = 
    [ Option "g" ["graphical"] (NoArg Graphical)
        "Start the graphical interface."
    , Option "f" ["file"]      (ReqArg File "File") 
        "Provide a file to load a Sudoku board from."
    , Option "V" ["version"]   (NoArg Version)
        "Print out version information."
    , Option "h" ["help"]      (NoArg Help)
        "Print this help message."
    ]


