
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Distributions.GrainSize.GrainDistributionGenerator
( genFullRandomGrainDistribution
, genPoint
, DistributedPoints(..)
, calcBoxSize
, defRatio
) where

import Control.Monad (replicateM, liftM, foldM)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.ST (runST)
import qualified Data.Vector as Vec
import Data.Vector (Vector, (!))
import Data.IORef
import Data.Random
import Data.Random.RVar
import Data.Random.Source.StdGen
import System.Random.Mersenne.Pure64

import qualified Hammer.Math.Vector as AlgLin
import Hammer.Math.Vector hiding (Vector)

import DeUni.DeWall

import VirMat.Distributions.GrainSize.StatTools
import VirMat.IO.Import.Types
import VirMat.Core.Sampling

data DistributedPoints a =
  DistributedPoints { box      :: Box a
                    , setPoint :: SetPoint a }


class (AlgLin.Vector v, AlgLin.Pointwise v)=> GenRandom v where
  type Ratio v :: *
  defRatio     :: Ratio v
  calcBoxSize  :: Double -> Ratio v -> Box v
  boxDim       :: Box v -> v
  genPoint     :: IORef PureMT -> RVar Double -> IO v



instance GenRandom Point2D where
  type Ratio Point2D = (Double, Double)

  defRatio = (1, 1)

  calcBoxSize avgVolume (xRatio, yRatio) = let
    refRatio = xRatio
    k1       = yRatio/refRatio
    a        = sqrt (avgVolume/k1)
    in Box2D {xMax2D = a, xMin2D = 0, yMax2D = a*k1, yMin2D = 0}

  boxDim Box2D{..} = let
    dx = (xMax2D - xMin2D)
    dy = (yMax2D - yMin2D)
    in Vec2 dx dy

  genPoint gen f = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    a <- sampleFrom gen f
    b <- sampleFrom gen f
    return $ Vec2 a b

instance GenRandom Point3D where
  type Ratio Point3D = (Double, Double, Double)

  defRatio = (1, 1, 1)

  calcBoxSize avgVolume (xRatio, yRatio, zRatio) =
    let
    refRatio = xRatio
    k1       = yRatio/refRatio
    k2       = zRatio/refRatio
    a        = (avgVolume/(k1*k2))**(1/3)
    in Box3D {xMax3D=a, xMin3D=0, yMax3D=a*k1, yMin3D=0, zMax3D=a*k2, zMin3D=0}

  boxDim Box3D{..} = let
    dx = (xMax3D - xMin3D)
    dy = (yMax3D - yMin3D)
    dz = (zMax3D - zMin3D)
    in Vec3 dx dy dz

  genPoint gen f = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    a <- sampleFrom gen f
    b <- sampleFrom gen f
    c <- sampleFrom gen f
    return $ Vec3 a b c

-- TODO remove ratio, add auto range, calc Weight
genFullRandomGrainDistribution:: (GenRandom v)=> Ratio v -> JobRequest -> IO (DistributedPoints v)
genFullRandomGrainDistribution ratio VoronoiJob{..} = case composeDist gsDist of
    Just mdist -> let
      (gsFunc, gsMean) = (mDistFunc mdist, mDistMean mdist)
      totalVolume      = gsMean * (fromIntegral targetNumber)
      box              = calcBoxSize totalVolume ratio
      delta            = boxDim box
      getPoint         = do
        -- test for integration sampling
        print $ integrateMDist mdist

        
        gen <- getRandomGen seed
        gs  <- genGrainSize mdist gen targetNumber
        ps  <- replicateM targetNumber (genPoint gen stdUniform)
        return $ zipWith (\gs p -> WPoint (gs / pi) (delta &! p)) gs ps
      in getPoint >>= return . (DistributedPoints box) . Vec.fromList
    Nothing -> error "[GrainDistributionGenerator] No target distrubution defined."

genGrainSize::MultiDist -> IORef PureMT -> Int -> IO [Double]
genGrainSize = sampleN

