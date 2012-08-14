
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Distributions.GrainSize.GrainDistributionGenerator
( genFullRandomGrainDistribution
, genPoint
, DistributedPoints(..)
, calcBox
, defRatio
, randomSO3
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
  type Ratio v    :: *
  defRatio        :: Ratio v
  calcBox         :: Double -> Ratio v -> Box v
  calcBoxFromDist :: Vector Double -> Ratio v -> Box v
  boxDim          :: Box v -> v
  genPoint        :: IORef PureMT -> RVar Double -> IO v

instance GenRandom Point2D where
  type Ratio Point2D = (Double, Double)

  defRatio = (1, 1)

  calcBox totalArea (xRatio, yRatio) = let
    refRatio  = xRatio
    k1        = yRatio/refRatio
    a         = sqrt (totalArea/k1)
    in Box2D {xMax2D = a, xMin2D = 0, yMax2D = a*k1, yMin2D = 0}
  
  calcBoxFromDist rs = let
    diaToArea d = d*d*pi/4
    totalArea   = Vec.foldl' (\acc d -> acc + diaToArea d) 0 rs
    in calcBox (1.1 * totalArea)
       
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

  calcBox totalVolume (xRatio, yRatio, zRatio) = let
    refRatio    = xRatio
    k1          = yRatio/refRatio
    k2          = zRatio/refRatio
    a           = (totalVolume/(k1*k2))**(1/3)
    in Box3D {xMax3D=a, xMin3D=0, yMax3D=a*k1, yMin3D=0, zMax3D=a*k2, zMin3D=0}
  
  calcBoxFromDist rs = let     
    diaToVol d  = d*d*d*pi/6
    totalVolume = Vec.foldl' (\acc d -> acc + diaToVol d) 0 rs
    in calcBox (1.1 * totalVolume)
       
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

-- | Generate a randomly spatial distribution of weighted points (spheres) where r = sqrt (weight)
-- and follows the diameter distribution given by @MultiDist in @VoronoiJob.    
genFullRandomGrainDistribution::(GenRandom v)=> Ratio v -> JobRequest -> IO (DistributedPoints v)
genFullRandomGrainDistribution ratio VoronoiJob{..} = case composeDist gsDist of
    Just mdist -> do
      gen <- getRandomGen seed
      ds  <- sampleN mdist gen targetNumber
      ps  <- Vec.replicateM targetNumber (genPoint gen stdUniform)
      let
        (gsFunc, gsMean) = (mDistFunc mdist, mDistMean mdist)
        -- Calculate the box size based on the total volume of sampled weighted points. 
        box              = calcBoxFromDist ds ratio
        delta            = boxDim box
        wpoints          = Vec.zipWith (\d p -> WPoint (d*d/4) (delta &! p)) ds ps
      return $ DistributedPoints box wpoints
    Nothing -> error "[GrainDistributionGenerator] No target distrubution defined."

randomSO3 :: JobRequest -> Int -> IO (Vector Vec3)
randomSO3 VoronoiJob{..} n = do
  gen <- getRandomGen seed
  let
    so3 = do
    p <- genPoint gen (uniform (-1) 1)
    let normP = norm p
      in if normP > 1
         then so3
         else return $ p &* (1/normP)
  Vec.replicateM n so3

