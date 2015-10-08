module Util.OtherSpec (main, spec) where

import Test.Hspec
import Util.Other

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let int0 = 0::Int
    let int1 = 1::Int
    let int3 = 3::Int
    describe "sToIntRange" $ do
        it "should return Nothing if the range is empty" $
            sToIntRange "a" [] `shouldBe` Nothing

        it "should return Nothing if the string is empty" $
            sToIntRange "" [] `shouldBe` Nothing

        it "should return an int if it is in the range" $
            sToIntRange "1" [1] `shouldBe` Just 1

        it "should return Nothing if the string is not in the range" $
            sToIntRange "2" [1] `shouldBe` Nothing

    describe "getMonadicRow" $
        it "should work with Just n" $ do
            getMonadicRow int1 (Just int1) `shouldBe` Just [1]
            getMonadicRow int3 (Just int1) `shouldBe` Just [1,1,1]
            getMonadicRow int0 (Just int1) `shouldBe` Just []

    describe "getMonadicGrid" $
        it "should work with Just n" $
            getMonadicGrid int1 (Just int1) `shouldBe` Just [[1]]
