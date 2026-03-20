module SamplingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import VirMat.Core.Sampling

spec :: Spec
spec = do
    describe "Normal distribution" $ do
        it "has correct area" $
            property $
                \s m v ->
                    v > 0 ==>
                        let dist = Normal s m v
                         in distArea dist == s

        it "has correct mean" $
            property $
                \s m v ->
                    v > 0 ==>
                        let dist = Normal s m v
                         in distMean dist == m

    describe "Uniform distribution" $ do
        it "has correct area" $
            property $
                \s m v ->
                    v > 0 ==>
                        let dist = Uniform s m v
                         in distArea dist == s

        it "has correct mean" $
            property $
                \s m v ->
                    v > 0 ==>
                        let dist = Uniform s m v
                         in distMean dist == m

    describe "composeDist" $ do
        it "composes areas correctly" $
            let d1 = CombDist (Normal 1.0 5.0 1.0)
                d2 = CombDist (Uniform 2.0 10.0 1.0)
                mDist = composeDist [d1, d2]
             in case mDist of
                    Just md -> mDistArea md `shouldBe` 3.0
                    Nothing -> expectationFailure "composeDist returned Nothing"

        it "composes functions correctly" $
            let d1 = CombDist (Normal 1.0 5.0 1.0)
                d2 = CombDist (Uniform 2.0 10.0 1.0)
                mDist = composeDist [d1, d2]
             in case mDist of
                    Just md -> mDistFunc md 5.0 `shouldBe` (getDistFunc d1 5.0 + getDistFunc d2 5.0)
                    Nothing -> expectationFailure "composeDist returned Nothing"
