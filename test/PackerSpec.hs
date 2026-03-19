{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PackerSpec (spec) where

import qualified Data.Vector as V
import DeUni.DeWall
import Linear.Vect
import Test.Hspec
import Test.QuickCheck
import VirMat.Core.Packer

instance Arbitrary (Box Vec2) where
    arbitrary = Box2D <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Box Vec3) where
    arbitrary = Box3D <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Vec2 Double) where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary

instance Arbitrary (Vec3 Double) where
    arbitrary = Vec3 <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
    describe "keepInBox" $ do
        it "keeps Vec2 within Box2D" $
            property $
                \(Box2D xMax xMin yMax yMin) (Vec2 x y) ->
                    let (Vec2 nx ny) = keepInBox (Box2D (max xMax xMin) (min xMax xMin) (max yMax yMin) (min yMax yMin)) (Vec2 x y)
                        xHigh = max xMax xMin
                        xLow = min xMax xMin
                        yHigh = max yMax yMin
                        yLow = min yMax yMin
                     in nx >= xLow && nx <= xHigh && ny >= yLow && ny <= yHigh

        it "keeps Vec3 within Box3D" $
            property $
                \(Box3D xMax xMin yMax yMin zMax zMin) (Vec3 x y z) ->
                    let (Vec3 nx ny nz) = keepInBox (Box3D (max xMax xMin) (min xMax xMin) (max yMax yMin) (min yMax yMin) (max zMax zMin) (min zMax zMin)) (Vec3 x y z)
                        xHigh = max xMax xMin
                        xLow = min xMax xMin
                        yHigh = max yMax yMin
                        yLow = min yMax yMin
                        zHigh = max zMax zMin
                        zLow = min zMax zMin
                     in nx >= xLow && nx <= xHigh && ny >= yLow && ny <= yHigh && nz >= zLow && nz <= zHigh

    describe "force" $ do
        it "is zero when points are at the same position and have zero radius" $
            let p1 = WPoint 0 (Vec2 0 0)
                p2 = WPoint 0 (Vec2 1e-10 0) -- use small delta to avoid NaN
                f = force p1 p2
             in vlen f `shouldSatisfy` (< 1e-8)

        it "is repulsive when points overlap" $
            let p1 = WPoint 1 (Vec2 0 0)
                p2 = WPoint 1 (Vec2 0.5 0)
                f = force p1 p2 -- Force ON p1 from p2
                (Vec2 fx _) = f
             in fx `shouldSatisfy` (< 0) -- p2 is to the right, repulsive force on p1 should be to the left
        it "is attractive when points are far apart" $
            let p1 = WPoint 1 (Vec2 0 0)
                p2 = WPoint 1 (Vec2 3 0)
                f = force p1 p2 -- Force ON p1 from p2
                (Vec2 fx _) = f
             in fx `shouldSatisfy` (> 0) -- p2 is to the right, attractive force on p1 should be to the right
