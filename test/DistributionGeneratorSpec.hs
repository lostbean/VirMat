{-# LANGUAGE TypeFamilies #-}

module DistributionGeneratorSpec (spec) where

import DeUni.DeWall
import Linear.Vect
import Test.Hspec
import Test.QuickCheck
import VirMat.Distributions.GrainSize.GrainDistributionGenerator

spec :: Spec
spec = do
    describe "GenRandom Vec2" $ do
        it "calcBox and boxExt are consistent" $
            property $
                \(Positive area) ->
                    let a = Area area
                        box = calcBox a (1, 1) :: Box Vec2
                        Area area' = boxExt box
                     in area' `shouldBeApprox` area

        it "boxDim and boxExt are consistent" $
            property $
                \(Positive w) (Positive h) ->
                    let box = Box2D w 0 h 0
                        Vec2 dw dh = boxDim box
                        Area area = boxExt box
                     in (dw * dh) `shouldBeApprox` area

    describe "GenRandom Vec3" $ do
        it "calcBox and boxExt are consistent" $
            property $
                \(Positive vol) ->
                    let v = Vol vol
                        box = calcBox v (1, 1, 1) :: Box Vec3
                        Vol vol' = boxExt box
                     in vol' `shouldBeApprox` vol

        it "boxDim and boxExt are consistent" $
            property $
                \(Positive w) (Positive h) (Positive d) ->
                    let box = Box3D w 0 h 0 d 0
                        Vec3 dw dh dd = boxDim box
                        Vol vol = boxExt box
                     in (dw * dh * dd) `shouldBeApprox` vol

shouldBeApprox :: Double -> Double -> Expectation
shouldBeApprox a b = abs (a - b) `shouldSatisfy` (< 1e-9)
