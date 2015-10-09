module Sudoku.Data.BoardSpec (main, spec) where

import Data.Maybe
import Test.Hspec

import Sudoku.Data.Board

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "isEmpty" $ do
        it "should return true given Empty" $
            isEmpty (fromJust (toSquare "0")) `shouldBe` True

        it "should return false given something else" $
            isEmpty (fromJust (toSquare "1")) `shouldBe` False
