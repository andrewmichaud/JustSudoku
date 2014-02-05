-- Andrew Michaud
-- 02/05/15
-- Model for MVC for Linux-Sudoku

import qualified Data.Vector as V
import Data.Maybe
import qualified Data.List

main = do
    -- get vars
    vars <- getLine

    let varsArray = words vars

    print varsArray

