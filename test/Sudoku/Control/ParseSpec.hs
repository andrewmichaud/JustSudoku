module Sudoku.Control.ParseSpec (main, spec) where

import Test.Hspec
import Sudoku.Control.Move
import Sudoku.Control.Parse
import Sudoku.Data.Board.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseMove" $ do
        context "Set" $ do
            it "should return Nothing given an invalid loc." $
                parseMove "sqq1" `shouldBe` Nothing

            it "should return Nothing given an invalid value." $
                parseMove "s11o" `shouldBe` Nothing

            it "should return Just Set with the right values." $ do
                parseMove "s111" `shouldBe` (Just $ Set "1" "1" "1")
                parseMove "s123" `shouldBe` (Just $ Set "1" "2" "3")

        it "should return Just Check given 'c'." $
            parseMove "c" `shouldBe` Just Check

        it "should return Just Quit given 'q'." $
            parseMove "q" `shouldBe` Just Quit

        it "should return Just Reset given 'r'." $
            parseMove "r" `shouldBe` Just Reset

        context "Erase" $ do
            it "should return Nothing given an invalid loc." $
                parseMove "eqq" `shouldBe` Nothing

            it "should return Just Erase given a valid loc." $
                parseMove "e11" `shouldBe` (Just $ Erase "1" "1")

        it "should return Nothing given an invalid move." $
            parseMove "fish" `shouldBe` Nothing

    describe "parseSquare" $ do
        it "should return an empty square given '0' or '_'" $ do
            parseSquare "0" `shouldBe` Just Empty
            parseSquare "_" `shouldBe` Just Empty

        it "should return a not-original n-square given 'n'" $ do
            parseSquare "1" `shouldBe` Just Val {value = V1, isOrig = False}
            parseSquare "2" `shouldBe` Just Val {value = V2, isOrig = False}
            parseSquare "3" `shouldBe` Just Val {value = V3, isOrig = False}
            parseSquare "4" `shouldBe` Just Val {value = V4, isOrig = False}
            parseSquare "5" `shouldBe` Just Val {value = V5, isOrig = False}
            parseSquare "6" `shouldBe` Just Val {value = V6, isOrig = False}
            parseSquare "7" `shouldBe` Just Val {value = V7, isOrig = False}
            parseSquare "8" `shouldBe` Just Val {value = V8, isOrig = False}
            parseSquare "9" `shouldBe` Just Val {value = V9, isOrig = False}

        it "should return Nothing given anything else" $
            parseSquare "fish" `shouldBe` Nothing
