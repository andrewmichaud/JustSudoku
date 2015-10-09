module Sudoku.Control.ParseSpec (main, spec) where

import Test.Hspec
import Sudoku.Control.Move
import Sudoku.Control.Parse
import Sudoku.Data.Board

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseMove" $ do
        it "should return Just Check given 'c'" $
            parseMove "c" `shouldBe` Just Check

        it "should return Just Quit given 'q'" $
            parseMove "q" `shouldBe` Just Quit

        it "should return Just Reset given 'r'" $
            parseMove "r" `shouldBe` Just Reset

    describe "parseSquare" $ do
        it "should return an empty square given '0'" $
            parseSquare "0" `shouldBe` toSquare "0"

        it "should return a not-original n-square given 'n'" $ do
            parseSquare "1" `shouldBe` toSquare "1"
            parseSquare "2" `shouldBe` toSquare "2"
            parseSquare "3" `shouldBe` toSquare "3"
            parseSquare "4" `shouldBe` toSquare "4"
            parseSquare "5" `shouldBe` toSquare "5"
            parseSquare "6" `shouldBe` toSquare "6"
            parseSquare "7" `shouldBe` toSquare "7"
            parseSquare "8" `shouldBe` toSquare "8"
            parseSquare "9" `shouldBe` toSquare "9"

        it "should return an empty square given _" $
            parseSquare "_" `shouldBe` toSquare "_"
