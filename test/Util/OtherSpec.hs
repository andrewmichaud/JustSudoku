module Util.OtherSpec (main, spec) where

import Test.Hspec
import Util.Other

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    let int0 = 0::Int
    let int1 = 1::Int
    describe "sToIntRange" $ do
        it "should return Nothing if the range is empty" $
            sToIntRange "a" [] `shouldBe` Nothing

        it "should return Nothing if the string is empty" $
            sToIntRange "" [1,2,3,4] `shouldBe` Nothing

        it "should return an int if it is in the range" $
            sToIntRange "1" [1] `shouldBe` Just 1

        it "should return Nothing if the string is not in the range" $
            sToIntRange "2" [1] `shouldBe` Nothing

    describe "getMonadicGrid" $ do
        it "should work with Just n" $
            getMonadicGrid int1 (Just int1) `shouldBe` Just [[1]]
        it "should return an empty grid of the right type given 0" $
            getMonadicGrid int0 (Just int1) `shouldBe` Just [[]]
