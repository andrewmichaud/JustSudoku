module Sudoku.Data.BoardSpec (main, spec) where

import qualified Data.Set as Set
import Test.Hspec

import Sudoku.Data.Board.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let empty = Empty
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

    let boardSquares = [replicate 9 empty,
                       [empty, val1True, empty, empty, val2True, empty, empty, val3True, empty],
                       replicate 9 empty,
                       replicate 9 empty,
                       [empty, val4True, empty, empty, val5True, empty, empty, val6True, empty],
                       replicate 9 empty,
                       replicate 9 empty,
                       [empty, val7True, empty, empty, val8True, empty, empty, val9True, empty],
                       replicate 9 empty]

    let boardSquaresString = "_ _ _ _ _ _ _ _ _\n" ++
                             "_ 1 _ _ 2 _ _ 3 _\n" ++
                             "_ _ _ _ _ _ _ _ _\n" ++
                             "_ _ _ _ _ _ _ _ _\n" ++
                             "_ 4 _ _ 5 _ _ 6 _\n" ++
                             "_ _ _ _ _ _ _ _ _\n" ++
                             "_ _ _ _ _ _ _ _ _\n" ++
                             "_ 7 _ _ 8 _ _ 9 _\n" ++
                             "_ _ _ _ _ _ _ _ _"

    let sixSquares = replicate 9 (replicate 9 val6False)

    let emptySquares = replicate 9 (replicate 9 empty)

    let testBoard = SudokuBoard boardSquares
    let sixBoard = SudokuBoard sixSquares
    let eBoard = SudokuBoard emptySquares

    let allLocations = concat [ [ Loc r c | c <- [0..8] ] | r <- [0..8]]

    let uglyTestBoard = "SudokuBoard " ++ show boardSquares

    let prettyTestBoard = "_ _ _   _ _ _   _ _ _\n" ++
                          "_ 1 _   _ 2 _   _ 3 _\n" ++
                          "_ _ _   _ _ _   _ _ _\n" ++
                          "                     \n" ++
                          "_ _ _   _ _ _   _ _ _\n" ++
                          "_ 4 _   _ 5 _   _ 6 _\n" ++
                          "_ _ _   _ _ _   _ _ _\n" ++
                          "                     \n" ++
                          "_ _ _   _ _ _   _ _ _\n" ++
                          "_ 7 _   _ 8 _   _ 9 _\n" ++
                          "_ _ _   _ _ _   _ _ _\n"

    describe "Location properties." $ do
        context "Equality:" $
            it "should hold for any two locations that are the same." $ do
                Loc 1 1 `shouldBe` Loc 1 1
                Loc 0 1 `shouldBe` Loc 0 1

        context "Inequality:" $
            it "should hold for any two locations that are not the same." $ do
                Loc 0 1 `shouldNotBe` Loc 5 3
                Loc 4 3 `shouldNotBe` Loc 2 7

        context "Show:" $
            it "should work correctly." $ do
                show (Loc 0 1) `shouldBe` "(0,1)"
                show (Loc 4 2) `shouldBe` "(4,2)"

    describe "Square properties." $ do
        context "Equality:" $ do
            it "should hold for Empty squares." $
                Empty `shouldBe` Empty

            it "should hold for non-empty squares." $ do
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

        context "Inequality:" $ do
            it "should hold for non-equal non-empty values." $ do
                val1True `shouldNotBe` val2True
                val2True `shouldNotBe` val1True
                val1True `shouldNotBe` val1False
                val1False `shouldNotBe` val1True

            it "should hold between Empty and non-empty values." $ do
                val1True `shouldNotBe` Empty
                Empty `shouldNotBe` val1True

        context "'value' data member:" $ do
            it "should match any equal value data member, isOrig should not matter." $ do
                value val1True `shouldBe` value val1False
                value val4True `shouldBe` value val4True

            it "should not match a non-equal value data member." $ do
                value val1True `shouldNotBe` value val8False
                value val4True `shouldNotBe` value val5True

        context "'isOrig' data member:" $ do
            it "should match an equal isOrig data member, for original squares." $ do
                isOrig val1True `shouldBe` isOrig val1True
                isOrig val2True `shouldBe` isOrig val2True
                isOrig val3True `shouldBe` isOrig val3True
                isOrig val4True `shouldBe` isOrig val4True
                isOrig val5True `shouldBe` isOrig val5True
                isOrig val6True `shouldBe` isOrig val6True
                isOrig val7True `shouldBe` isOrig val7True
                isOrig val8True `shouldBe` isOrig val8True
                isOrig val9True `shouldBe` isOrig val9True

            it "should match an equal isOrig data member, for non-original squares." $ do
                isOrig val1False `shouldBe` isOrig val1False
                isOrig val2False `shouldBe` isOrig val2False
                isOrig val3False `shouldBe` isOrig val3False
                isOrig val4False `shouldBe` isOrig val4False
                isOrig val5False `shouldBe` isOrig val5False
                isOrig val6False `shouldBe` isOrig val6False
                isOrig val7False `shouldBe` isOrig val7False
                isOrig val8False `shouldBe` isOrig val8False
                isOrig val9False `shouldBe` isOrig val9False

            it "squares where one is original and one is not should not have matching isOrig" $ do
                value val1True `shouldNotBe` value val8False
                value val4False `shouldNotBe` value val5True

        context "Show:" $
            it "should show correctly" $ do
                show val1True `shouldBe` "1"
                show val2False `shouldBe` "2"
                show empty `shouldBe` "_"

    describe "SqVal properties." $ do
        context "Equality:" $ do
            it "should hold between a value and itself." $ do
                V1 `shouldBe` V1
                V2 `shouldBe` V2
                V3 `shouldBe` V3
                V4 `shouldBe` V4
                V5 `shouldBe` V5
                V6 `shouldBe` V6
                V7 `shouldBe` V7
                V8 `shouldBe` V8
                V9 `shouldBe` V9

            it "should not hold between two different values." $
                V1 `shouldNotBe` V2

        context "Ordering:" $ do
            it "should hold in the 'less than' sense." $ do
                V1 `shouldSatisfy` ( < V2)
                V2 `shouldSatisfy` ( < V3)
                V3 `shouldSatisfy` ( < V4)
                V4 `shouldSatisfy` ( < V5)
                V5 `shouldSatisfy` ( < V6)
                V6 `shouldSatisfy` ( < V7)
                V7 `shouldSatisfy` ( < V8)
                V8 `shouldSatisfy` ( < V9)

            it "should hold in the 'greater than' sense." $ do
                V9 `shouldSatisfy` ( > V8)
                V8 `shouldSatisfy` ( > V7)
                V7 `shouldSatisfy` ( > V6)
                V6 `shouldSatisfy` ( > V5)
                V5 `shouldSatisfy` ( > V4)
                V4 `shouldSatisfy` ( > V3)
                V3 `shouldSatisfy` ( > V2)
                V2 `shouldSatisfy` ( > V1)

    describe "SudokuBoard." $ do
        it "should Show properly." $
            show testBoard `shouldBe` uglyTestBoard

        it "should implement equality correctly." $ do
            testBoard `shouldBe` testBoard
            sixBoard `shouldBe` sixBoard
            eBoard `shouldBe` eBoard

        it "should implement inequality correctly." $ do
            testBoard `shouldNotBe` sixBoard
            eBoard `shouldNotBe` testBoard
            sixBoard `shouldNotBe` eBoard

    describe "isEmpty:" $ do
        it "should return true given Empty." $
            isEmpty Empty `shouldBe` True

        it "should return false given something else." $ do
            isEmpty Val {value = V1, isOrig = False} `shouldBe` False
            isEmpty Val {value = V1, isOrig = True} `shouldBe` False

    describe "toSquare:" $ do
        it "should return Just Empty given '0' or '_'." $ do
            toSquare False "0" `shouldBe` Just Empty
            toSquare False "_" `shouldBe` Just Empty

        it "should return Just Square for a number given the number." $ do
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

        it "should return Nothing given anything else." $
            toSquare False "fish" `shouldBe` Nothing

    describe "toLocation:" $ do
        it "should return Just Loc if given coordinates in 0-8 inclusive." $ do
            toLocation "0" "2" `shouldBe` Just (Loc 0 2)
            toLocation "8" "8" `shouldBe` Just (Loc 8 8)

        it "should return Nothing otherwise." $ do
            toLocation "22" "33" `shouldBe` Nothing
            toLocation "fish" "butts" `shouldBe` Nothing

    describe "tupleToLocation:" $
        it "should produce a Loc given an int tuple." $ do
            tupleToLocation (1, 2) `shouldBe` Loc 1 2
            tupleToLocation (20, 4) `shouldBe` Loc 20 4

    describe "emptyBoard:" $
        it "should provide an empty board." $
            emptyBoard `shouldBe` eBoard

    describe "parseStringToBoard:" $
        it "should parse a string into a board correctly." $
            parseStringToBoard boardSquaresString `shouldBe` testBoard

    describe "resetBoard:" $ do
        it "should not change any original squares." $
            resetBoard testBoard `shouldBe` testBoard

        it "should change any non-original squares into Empty squares." $
            resetBoard sixBoard `shouldBe` eBoard

        it "should be the identity operation on an empty board." $
            resetBoard eBoard `shouldBe` eBoard

    describe "eraseLocation:" $ do
        it "should not change an original square's value." $ do
            eraseLocation val1True `shouldBe` val1True
            eraseLocation val2True `shouldBe` val2True
            eraseLocation val3True `shouldBe` val3True

        it "should erase a non-original square's value." $ do
            eraseLocation val1False `shouldBe` empty
            eraseLocation val2False `shouldBe` empty
            eraseLocation val3False `shouldBe` empty

        it "should be the identity operation on an Empty square." $
            eraseLocation empty `shouldBe` empty

    describe "getBoardValue:" $
        it "should return the correct value given any valid coordinates." $ do
            getBoardValue eBoard (Loc 0 0) `shouldBe` empty
            getBoardValue testBoard (Loc 1 1) `shouldBe` val1True
            getBoardValue sixBoard (Loc 5 6) `shouldBe` val6False

    describe "setBoardValue:" $ do
        it "should set any non-original value you want on an empty or non-original square." $ do
            let alteredBoard = setBoardValue eBoard (Loc 0 0) val6False
            let alteredBoard2 = setBoardValue sixBoard (Loc 0 0) empty
            let restoredBoard = setBoardValue alteredBoard (Loc 0 0) empty
            getBoardValue alteredBoard (Loc 0 0) `shouldBe` val6False
            getBoardValue alteredBoard2 (Loc 0 0) `shouldBe` empty
            getBoardValue restoredBoard (Loc 0 0) `shouldBe` empty

        it "should not let you set a value on an original square." $ do
            let notAlteredBoard = setBoardValue testBoard (Loc 1 1) val4False
            getBoardValue notAlteredBoard  (Loc 1 1) `shouldNotBe` val4False
            getBoardValue notAlteredBoard  (Loc 1 1) `shouldBe` val1True

    describe "eraseBoardValue:" $ do
        it "should let you erase any non-original square." $ do
            let alteredBoard = eraseBoardValue sixBoard (Loc 6 6)
            let alteredBoard2 = setBoardValue eBoard (Loc 1 1) val1False
            let alteredBoard3 = eraseBoardValue alteredBoard2 (Loc 1 1)
            getBoardValue alteredBoard (Loc 6 6) `shouldBe` empty
            getBoardValue alteredBoard2 (Loc 1 1) `shouldBe` val1False
            getBoardValue alteredBoard3 (Loc 1 1) `shouldBe` empty

        it "should not let you erase non-original squares." $ do
            let alteredBoard = eraseBoardValue testBoard (Loc 1 1)
            let alteredBoard2 = eraseBoardValue testBoard (Loc 4 1)
            let alteredBoard3 = eraseBoardValue testBoard (Loc 7 7)
            getBoardValue alteredBoard (Loc 1 1) `shouldBe` val1True
            getBoardValue alteredBoard2 (Loc 4 1) `shouldBe` val4True
            getBoardValue alteredBoard3 (Loc 7 7) `shouldBe` val9True

    describe "checkBoard:" $ do
        it "should say an empty board is valid." $
            checkBoard eBoard `shouldBe` Set.empty

        it "should say a valid board is valid." $
            checkBoard testBoard `shouldBe` Set.empty

        it "should not say a board of all 6 is valid." $
            checkBoard sixBoard `shouldBe` Set.fromList allLocations

    describe "prettyPrint:" $
        it "should work correctly." $
            prettyPrint testBoard `shouldBe` prettyTestBoard

    describe "toIndex:" $ do
        it "should provide Just Int if given something in 0..8." $ do
            toIndex "0" `shouldBe` Just 0
            toIndex "1" `shouldBe` Just 1
            toIndex "2" `shouldBe` Just 2
            toIndex "3" `shouldBe` Just 3
            toIndex "4" `shouldBe` Just 4
            toIndex "5" `shouldBe` Just 5
            toIndex "6" `shouldBe` Just 6
            toIndex "7" `shouldBe` Just 7
            toIndex "8" `shouldBe` Just 8

        it "should provide Nothing in any other case." $ do
            toIndex "-1" `shouldBe` Nothing
            toIndex "9" `shouldBe` Nothing
            toIndex "fish" `shouldBe` Nothing

    describe "checkRows:" $ do
        it "should return an empty set for the empty board." $
            checkRows eBoard `shouldBe` Set.empty

        it "should return an empty set for a valid board." $
            checkRows testBoard `shouldBe` Set.empty

    describe "checkCols:" $ do
        it "should return an empty set for the empty board." $
            checkCols eBoard `shouldBe` Set.empty

        it "should return an empty set for a valid board." $
            checkCols testBoard `shouldBe` Set.empty

    describe "checkSubgrids:" $ do
        it "should return an empty set for the empty board." $
            checkSubgrids eBoard `shouldBe` Set.empty

        it "should return an empty set for a valid board." $
            checkSubgrids testBoard `shouldBe` Set.empty

    describe "checkList:" $
        it "should return an empty set for an empty list." $
            checkList [] `shouldBe` Set.empty
