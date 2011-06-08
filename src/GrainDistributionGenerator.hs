
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GrainDistributionGenerator (
getGrainBoxDistribution,
getFullRandomGrainDistribution,
getRandomGen,
DistributedPoints(..)
) where

import Control.Monad (replicateM, liftM, foldM)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.ST (runST)
import Data.Array.Vector (toU)
import Data.Random
import Data.Random.RVar
import Data.Random.Source.StdGen
import Data.Random.Internal.Primitives

import Data.IORef
import Data.Vec hiding (map, take, zipWith, length, head)
import System.Random.Mersenne.Pure64
import Data.Array.IArray (listArray, indices, Array)


import DelaunayReverseOnion (Box(..), isInBox)
import CommandLineInput (RandomSeed(..))


import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x



data DistributedPoints = DistributedPoints Box [Vec3D]



getGrainBoxDistribution::IORef PureMT -> Int -> Double -> Double -> (Double, Double, Double) -> IO DistributedPoints
getGrainBoxDistribution gen targetNGrains avgVolume stdDev ratio = result
    where
        nGrainsIn1D = calcNGrainsIn1D targetNGrains
        floatNGrains1D = fromIntegral nGrainsIn1D

        box = calcBoxSize avgVolume ratio
        dx = (xMax box - xMin box)
        dy = (yMax box - yMin box)
        dz = (zMax box - zMin box)

        lx = dx*floatNGrains1D
        ly = dy*floatNGrains1D
        lz = dz*floatNGrains1D

        fullBox = Box {xMax=lx,xMin=0,yMax=ly,yMin=0,zMax=lz,zMin=0}

        grainInBox::Int -> Int -> Int -> Vec3D
        grainInBox a b c = Vec3D ((fromIntegral a)*dx+dx/2) ((fromIntegral b)*dy+dy/2) ((fromIntegral c)*dz+dz/2)

        putGrainInBox = [grainInBox ix iy iz | ix <- [0..nGrainsIn1D-1], iy <- [0..nGrainsIn1D-1], iz <- [0..nGrainsIn1D-1]]

        getPoint::Vec3D -> IO Vec3D
        getPoint x = (getWavePointList gen (normal 0 stdDev) (dx,dy,dz)) >>= (\a -> return $ a + x)

        addVarianceInsideBox x = iterateUntil (isInBox fullBox) (getPoint x)

        result = mapM addVarianceInsideBox putGrainInBox >>= return.(DistributedPoints fullBox)



getFullRandomGrainDistribution::IORef PureMT -> Int -> Double -> (Double, Double, Double) -> IO DistributedPoints
getFullRandomGrainDistribution gen targetNGrains avgVolume ratio = result
    where
        totalVolume = avgVolume*(fromIntegral targetNGrains)
        box = calcBoxSize totalVolume ratio
        dx = (xMax box - xMin box)
        dy = (yMax box - yMin box)
        dz = (zMax box - zMin box)

        getPoint = getWavePointList gen stdUniform (dx, dy, dz)

        result = replicateM targetNGrains getPoint >>= return.(DistributedPoints box)

calcNGrainsIn1D::Int -> Int
calcNGrainsIn1D n = round $ (fromIntegral n)**(1/3)


calcBoxSize::Double -> (Double, Double, Double) -> Box
calcBoxSize avgVolume (xRatio, yRatio, zRatio) = Box {xMax,xMin,yMax,yMin,zMax,zMin}
    where
        cubeBoxEdge = avgVolume ** (1/3)

        refRatio = xRatio
        --k0 = xRatio/refRatio = 1
        k1 = yRatio/refRatio
        k2 = zRatio/refRatio
        a = (avgVolume/(k1*k2))**(1/3)

        xMax = a
        yMax = a*k1
        zMax = a*k2
        xMin = 0
        yMin = 0
        zMin = 0


getWavePointList::IORef PureMT -> RVar Double -> (Double, Double, Double) -> IO Vec3D
getWavePointList gen f (dx, dy,dz) = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    a <- sampleFrom gen f
    b <- sampleFrom gen f
    c <- sampleFrom gen f
    return $ Vec3D (a*dx) (b*dy) (c*dz)


getRandomGen::RandomSeed -> IO (IORef PureMT)
getRandomGen x = case x of
    None -> newPureMT >>= newIORef
    (Seed seed) ->  (return $ pureMT (fromIntegral seed)) >>= newIORef
