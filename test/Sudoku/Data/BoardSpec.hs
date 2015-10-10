module Sudoku.Data.BoardSpec (main, spec) where

import Test.Hspec

import Sudoku.Data.Board.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let val1True = Val {value = V1, isOrig = True}
    let val2True = Val {value = V2, isOrig = True}
    let val3True = Val {value = V3, isOrig = True}
    let val4True = Val {value = V4, isOrig = True}
    let val5True = Val {value = V5, isOrig = True}
    let val6True = Val {value = V6, isOrig = True}
    let val7True = Val {value = V7, isOrig = True}
    let val8True = Val {value = V8, isOrig = True}
    let val9True = Val {value = V9, isOrig = True}
    let val1False = Val {value = V1, isOrig = False}
    let val2False = Val {value = V2, isOrig = False}
    let val3False = Val {value = V3, isOrig = False}
    let val4False = Val {value = V4, isOrig = False}
    let val5False = Val {value = V5, isOrig = False}
    let val6False = Val {value = V6, isOrig = False}
    let val7False = Val {value = V7, isOrig = False}
    let val8False = Val {value = V8, isOrig = False}
    let val9False = Val {value = V9, isOrig = False}
    describe "isEmpty" $ do
        it "should return true given Empty" $
            isEmpty Empty `shouldBe` True

        it "should return false given something else" $ do
            isEmpty Val {value = V1, isOrig = False} `shouldBe` False
            isEmpty Val {value = V1, isOrig = True} `shouldBe` False

    describe "toSquare" $ do
        it "should return Just Empty given '0' or '_'" $ do
            toSquare False "0" `shouldBe` Just Empty
            toSquare False "_" `shouldBe` Just Empty

        it "should return Just Square for a number given the number" $ do
            toSquare True "1" `shouldBe` Just val1True
            toSquare True "2" `shouldBe` Just val2True
            toSquare True "3" `shouldBe` Just val3True
            toSquare True "4" `shouldBe` Just val4True
            toSquare True "5" `shouldBe` Just val5True
            toSquare True "6" `shouldBe` Just val6True
            toSquare True "7" `shouldBe` Just val7True
            toSquare True "8" `shouldBe` Just val8True
            toSquare True "9" `shouldBe` Just val9True
            toSquare False "1" `shouldBe` Just val1False
            toSquare False "2" `shouldBe` Just val2False
            toSquare False "3" `shouldBe` Just val3False
            toSquare False "4" `shouldBe` Just val4False
            toSquare False "5" `shouldBe` Just val5False
            toSquare False "6" `shouldBe` Just val6False
            toSquare False "7" `shouldBe` Just val7False
            toSquare False "8" `shouldBe` Just val8False
            toSquare False "9" `shouldBe` Just val9False

        it "should return Nothing given anything else" $
            toSquare False "fish" `shouldBe` Nothing

    describe "toIndex" $ do
        it "should provide Just Int if given something in 0..8" $ do
            toIndex "0" `shouldBe` Just 0
            toIndex "1" `shouldBe` Just 1
            toIndex "2" `shouldBe` Just 2
            toIndex "3" `shouldBe` Just 3
            toIndex "4" `shouldBe` Just 4
            toIndex "5" `shouldBe` Just 5
            toIndex "6" `shouldBe` Just 6
            toIndex "7" `shouldBe` Just 7
            toIndex "8" `shouldBe` Just 8

        it "should provide Nothing in any other case" $ do
            toIndex "-1" `shouldBe` Nothing
            toIndex "9" `shouldBe` Nothing
            toIndex "fish" `shouldBe` Nothing

    describe "Testing Square properties" $
        context "Equality" $ do
            it "Empty should equal Empty" $
                Empty `shouldBe` Empty

            it "values that are the same should be equal" $ do
                val1True `shouldBe` val1True
                val2True `shouldBe` val2True
                val3True `shouldBe` val3True
                val4True `shouldBe` val4True
                val5True `shouldBe` val5True
                val6True `shouldBe` val6True
                val7True `shouldBe` val7True
                val8True `shouldBe` val8True
                val9True `shouldBe` val9True
                val1False `shouldBe` val1False
                val2False `shouldBe` val2False
                val3False `shouldBe` val3False
                val4False `shouldBe` val4False
                val5False `shouldBe` val5False
                val6False `shouldBe` val6False
                val7False `shouldBe` val7False
                val8False `shouldBe` val8False
                val9False `shouldBe` val9False

            it "values that are not should not" $ do
                val1True `shouldNotBe` val2True
                val2True `shouldNotBe` val1True
                val1True `shouldNotBe` Empty
                Empty `shouldNotBe` val1True
                val1True `shouldNotBe` val1False
                val1False `shouldNotBe` val1True

    describe "Testing SqVal properties..." $ do
        context "(In)Equality" $ do
            it "Values should equal themselves." $ do
                V1 `shouldBe` V1
                V2 `shouldBe` V2
                V3 `shouldBe` V3
                V4 `shouldBe` V4
                V5 `shouldBe` V5
                V6 `shouldBe` V6
                V7 `shouldBe` V7
                V8 `shouldBe` V8
                V9 `shouldBe` V9

            it "Values should not equal other values." $
                V1 `shouldNotBe` V2

        context "Ordering" $
            it "should work." $ do
                V1 `shouldSatisfy` ( < V2)
                V2 `shouldSatisfy` ( < V3)
                V3 `shouldSatisfy` ( < V4)
                V4 `shouldSatisfy` ( < V5)
                V5 `shouldSatisfy` ( < V6)
                V6 `shouldSatisfy` ( < V7)
                V7 `shouldSatisfy` ( < V8)
                V8 `shouldSatisfy` ( < V9)
