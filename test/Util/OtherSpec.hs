module Util.OtherSpec (main, spec) where

import Test.Hspec
import Util.Other

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    let int0 = 0::Int
    let int1 = 1::Int

    describe "addToTupleIf" $ do
        it "should not add int if only the first condition is true." $ do
            addToTupleIf 2 (==2) (==2) (2, 3) `shouldBe` (2, 3)
            addToTupleIf 2 (==2) (const False) (2, 3) `shouldBe` (2, 3)

        it "should not add int if only the second condition is true." $ do
            addToTupleIf 2 (==3) (==3) (2, 3) `shouldBe` (2, 3)
            addToTupleIf 2 (const False) (==3) (2, 3) `shouldBe` (2, 3)

        it "should not add int if neither condition is true." $ do
            addToTupleIf 2 (==5) (==5) (2, 3) `shouldBe` (2, 3)
            addToTupleIf 2 (const False) (const False) (2, 3) `shouldBe` (2, 3)

        it "should add int if both conditions are true." $ do
            addToTupleIf 2 (==2) (==3) (2, 3) `shouldBe` (4, 5)
            addToTupleIf 2 (const True) (const True) (2, 3) `shouldBe` (4, 5)

    describe "incrementTupleIf" $ do
        it "should not increment if only the first condition is true." $ do
            incrementTupleIf (==2) (==2) (2, 3) `shouldBe` (2, 3)
            incrementTupleIf (==2) (const False) (2, 3) `shouldBe` (2, 3)

        it "should not increment if only the second condition is true." $ do
            incrementTupleIf (==3) (==3) (2, 3) `shouldBe` (2, 3)
            incrementTupleIf (const False) (==3) (2, 3) `shouldBe` (2, 3)

        it "should not increment if neither condition is true." $ do
            incrementTupleIf (==5) (==5) (2, 3) `shouldBe` (2, 3)
            incrementTupleIf (const False) (const False) (2, 3) `shouldBe` (2, 3)

        it "should increment if both conditions are true." $ do
            incrementTupleIf (==2) (==3) (2, 3) `shouldBe` (3, 4)
            incrementTupleIf (const True) (const True) (2, 3) `shouldBe` (3, 4)

    describe "getMonadicGrid" $ do
        it "should work with Just n." $
            getMonadicGrid int1 (Just int1) `shouldBe` Just [[1]]

        it "should return an empty grid of the right type given 0" $
            getMonadicGrid int0 (Just int1) `shouldBe` Just [[]]

    describe "sToIntRange" $ do
        it "should return Nothing if the range is empty" $
            sToIntRange "a" [] `shouldBe` Nothing

        it "should return Nothing if the string is empty" $
            sToIntRange "" [1,2,3,4] `shouldBe` Nothing

        it "should return an int if it is in the range" $
            sToIntRange "1" [1] `shouldBe` Just 1

        it "should return Nothing if the string is not in the range" $
            sToIntRange "2" [1] `shouldBe` Nothing
