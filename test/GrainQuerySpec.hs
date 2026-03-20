module GrainQuerySpec (spec) where

import Linear.Vect
import Test.Hspec
import VirMat.Distributions.GrainSize.GrainQuery

spec :: Spec
spec = do
    describe "triangleArea" $ do
        it "calculates correct area for a right triangle" $
            let a = Vec2 1 0
                b = Vec2 0 1
                c = Vec2 0 0
                Area area = triangleArea a b c
             in area `shouldBeApprox` 0.5

        it "is zero for collinear points" $
            let a = Vec2 0 0
                b = Vec2 1 1
                c = Vec2 2 2
                Area area = triangleArea a b c
             in area `shouldBeApprox` 0.0

    describe "tetrahedronVolume" $ do
        it "calculates correct volume for a unit tetrahedron" $
            let a = Vec3 1 0 0
                b = Vec3 0 1 0
                c = Vec3 0 0 1
                d = Vec3 0 0 0
                Volume vol = tetrahedronVolume a b c d
             in vol `shouldBeApprox` (1 / 6)

        it "is zero for coplanar points" $
            let a = Vec3 0 0 0
                b = Vec3 1 0 0
                c = Vec3 0 1 0
                d = Vec3 1 1 0
                Volume vol = tetrahedronVolume a b c d
             in vol `shouldBeApprox` 0.0

shouldBeApprox :: Double -> Double -> Expectation
shouldBeApprox a b = abs (a - b) `shouldSatisfy` (< 1e-9)
