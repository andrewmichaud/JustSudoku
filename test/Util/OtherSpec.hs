module Util.OtherSpec (main, spec) where

import Test.Hspec
import Util.Other

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
        it "should work with Just 1" $
            getMonadicRow 1 (Just 1) `shouldBe` Just [1]

    describe "getMonadicGrid" $
        it "should work with Just 1" $
            getMonadicGrid 1 (Just 1) `shouldBe` Just [[1]]
