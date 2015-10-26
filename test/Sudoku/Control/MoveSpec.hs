module Sudoku.Control.MoveSpec (main, spec) where

import Data.Set(singleton)
import Test.Hspec

import Sudoku.Control.Move
import Sudoku.Data.Board.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Move properties:" $ do
        let set   = Set "0" "1" "1"
        let set2  = Set "3" "4" "5"
        let check = Check
        let erase = Erase "0" "1"
        let erase2 = Erase "0" "4"
        let reset = Reset
        let quit  = Quit

        context "Show:" $ do
            it "should work for Set" $ do
                show set `shouldBe` "Set \"0\" \"1\" \"1\""
                show set2 `shouldBe` "Set \"3\" \"4\" \"5\""

            it "should work for Check" $
                show check `shouldBe` "Check"

            it "should work for Erase" $ do
                show erase `shouldBe` "Erase \"0\" \"1\""
                show erase2 `shouldBe` "Erase \"0\" \"4\""

            it "should work for Reset" $
                show reset `shouldBe` "Reset"

            it "should work for Quit" $
                show quit `shouldBe` "Quit"

        context "Equality:" $ do
            it "should hold between moves that are the same." $ do
                set `shouldBe` set
                check `shouldBe` check
                erase `shouldBe` erase
                reset `shouldBe` reset
                quit `shouldBe` quit

            it "should not hold between different kinds of moves." $ do
                set `shouldNotBe` check
                set `shouldNotBe` erase
                set `shouldNotBe` reset
                set `shouldNotBe` quit
                check `shouldNotBe` erase
                check `shouldNotBe` reset
                check `shouldNotBe` quit
                erase `shouldNotBe` reset
                erase `shouldNotBe` quit
                reset `shouldNotBe` quit

            it "should not hold between moves with different data members." $ do
                set `shouldNotBe` set2
                erase `shouldNotBe` erase2

    describe "MoveError properties:" $ do
        let r = 100::Int
        let rs = show r
        let c = 100::Int
        let cs = show c
        let v = 100::Int
        let vs = show v
        let squares = singleton $ Loc 1 2
        let ssquares = show squares

        context "Show:" $ do
            context "NaNError:" $
                it "should display correctly." $
                    show (NaNError "q") `shouldBe` ("Value " ++ "q" ++ " is not a number.")

            it "OutOfBoundsError should display correctly." $
                show (OutOfBoundsError r c) `shouldBe` ("Square (" ++ rs ++ ", " ++ cs ++
                                                        ") is out of bounds.")

            it "InvalidValueError should display correctly." $
                show (InvalidValueError v) `shouldBe` ("Value " ++ vs ++ " is invalid.")

            it "InvalidBoardError should display correctly," $
                show (InvalidBoardError squares) `shouldBe` ("Board is invalid. " ++
                                                             "Invalid squares: " ++ ssquares)
            it "OtherError should display correctly." $
                show (OtherError "fish") `shouldBe` ("General error: " ++ "fish")

            it "QuitError." $
                show QuitError `shouldBe` "Asked or required to quit!"
