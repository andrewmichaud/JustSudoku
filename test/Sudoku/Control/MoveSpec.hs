module Sudoku.Control.MoveSpec (main, spec) where

import Test.Hspec
import Sudoku.Control.Move.Internal
import Sudoku.Data.Board.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Checking Move properties" $
        context "Checking (In)Equality" $ do
            it "A move should equal the same move." $ do
                Check `shouldBe` Check
                Erase "0" "1" `shouldBe` Erase "0" "1"
                Reset `shouldBe` Reset
                Quit `shouldBe` Quit

            it "A move should not equal a different move." $ do
                Check `shouldNotBe` Erase "0" "1"
                Check `shouldNotBe` Reset
                Check `shouldNotBe` Quit
                Erase "0" "1" `shouldNotBe` Reset
                Erase "0" "1" `shouldNotBe` Quit
                Reset `shouldNotBe` Quit

    describe "Checkng MoveError properties" $ do
        let r = 100::Int
        let rs = show r
        let c = 100::Int
        let cs = show c
        let v = 100::Int
        let vs = show v
        let squares = [Loc 1 2]
        let ssquares = show squares
        context "Checking Show" $ do
            it "NaNError should display correctly" $
                show (NaNError "q") `shouldBe` ("Value " ++ "q" ++ " is not a number.")

            it "OutOfBoundsError should display correctly" $
                show (OutOfBoundsError r c) `shouldBe` ("Square (" ++ rs ++ ", " ++ cs ++
                                                        ") is out of bounds.")

            it "InvalidValueError should display correctly" $
                show (InvalidValueError v) `shouldBe` ("Value " ++ vs ++ " is invalid.")

            it "InvalidBoardError should display correctly" $
                show (InvalidBoardError squares) `shouldBe` ("Board is invalid. " ++
                                                             "Invalid squares: " ++ ssquares)
            it "OtherError should display correctly" $
                show (OtherError "fish") `shouldBe` ("General error: " ++ "fish")

            it "QuitError" $
                show QuitError `shouldBe` "Asked or required to quit!"
