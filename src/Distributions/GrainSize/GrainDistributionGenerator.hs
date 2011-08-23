
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Distributions.GrainSize.GrainDistributionGenerator
( getGrainBoxDistribution
, getFullRandomGrainDistribution
, getWavePointList
, getRandomGen
, DistributedPoints(..)
, Point
, calcBoxSize
) where

import Control.Monad (replicateM, liftM, foldM)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.ST (runST)
import Data.Array.Diff (listArray, (!), elems, bounds, (//), DiffArray)
import Data.IORef
import Data.Random
import Data.Random.RVar
import Data.Random.Source.StdGen
import Data.Vec hiding (map, take, zipWith, length, head)
import System.Random.Mersenne.Pure64

import Math.DeUni (Box(..), isInBox, PointPointer (..), SetPoint)
import Math.DeUni (Box(..), isInBox)
import Douane.Import.Prompt.CommandLineInput (RandomSeed(..))


import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

type Point = Vec3D

data DistributedPoints = DistributedPoints { box::Box
                                           , setPoint::SetPoint }

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
        
        toArr = listArray (1::PointPointer, fromIntegral targetNGrains)
        result = mapM addVarianceInsideBox putGrainInBox >>= return.(DistributedPoints fullBox).toArr



getFullRandomGrainDistribution::IORef PureMT -> Int -> Double -> (Double, Double, Double) -> IO DistributedPoints
getFullRandomGrainDistribution gen targetNGrains avgVolume ratio = result
    where
        totalVolume = avgVolume*(fromIntegral targetNGrains)
        box = calcBoxSize totalVolume ratio
        dx = (xMax box - xMin box)
        dy = (yMax box - yMin box)
        dz = (zMax box - zMin box)

        getPoint = getWavePointList gen stdUniform (dx, dy, dz)
        
        toArr = listArray (1::PointPointer, fromIntegral targetNGrains)
        result = replicateM targetNGrains getPoint >>= return.(DistributedPoints box).toArr


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
    NoSeed -> newPureMT >>= newIORef
    (Seed seed) ->  (return $ pureMT (fromIntegral seed)) >>= newIORef