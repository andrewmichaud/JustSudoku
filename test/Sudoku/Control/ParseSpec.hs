module Sudoku.Control.ParseSpec (main, spec) where

import Test.Hspec
import Sudoku.Control.Move
import Sudoku.Control.Parse

main :: IO ()
main = hspec spec

spec :: Spec
spec =
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
