{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VirMat.Distributions.GrainSize.GrainQuery
  ( getGrainArea
  , getGrainVolume
  , getTotalGrainVolume
  , getAverageGrainVolume
  , getStdDeviationGrainVolume
  , getTotalGrainArea
  , getFaceAreaFracHist
  , Volume (getVolume)
  , Area   (getArea)
  ) where

import qualified Data.IntMap as IM
  
import           Data.List (foldl')

import           DeUni.Dim3.Base3D
import           DeUni.Types
import           Hammer.Math.Algebra

import           VirMat.Core.VoronoiBuilder


newtype Area   = Area
                 { getArea :: Double
                 } deriving (Eq, Ord, Num, Fractional, Show)
                            
newtype Volume = Volume
                 { getVolume :: Double
                 } deriving (Eq, Ord, Num, Fractional, Show)

getGrainVolume :: VoronoiGrain Point3D -> Volume
getGrainVolume grain = let
  volume        = foldl' (\a b -> a + (prismVolume b)) 0 (faces grain)
  prismVolume f = (normal &. centroid) * (getArea area)
    where area   = getFaceArea f
          normal = getNormalToFace grain f
          centroid = getCentroid (map (circumSphereCenter.snd) (edges f))
  in  Volume $ abs (volume / 3)
      
getGrainArea :: VoronoiGrain Point3D -> Area
getGrainArea grain = foldl' (\a b -> a + (getArea b)) (Area 0) (faces grain)
  where getArea x  = getFaceArea x

-- | Calculate the area of each face using a signed sum of triangles.
-- It assume that the edge pairs are given in a oriented sequnce,
-- e.g. (P0,P1,P2 .... Pn) 
getFaceArea :: (PointND a)=> VoronoiFace a -> Area
getFaceArea face = let
  ps         = map (circumOrigin.snd) (edges face)
  errFace    = error "[GrainQuery] Malformed face: face with less than 3 points."
  area ab ac = Area $ 0.5 * sqrt (normsqr ab * normsqr ac - (ab &. ac)^2)
  -- the result will not make sense if the list conteins less than 3 points
  sumTri (a:as) = thunkSum as
    where
      thunkSum (b:c:xs) = area (b &- a) (c &- a)  + thunkSum (c:xs)
      -- boundary condition for a non empty list set
      thunkSum [a]        = 0
      thunkSum []         = errFace
  sumTri _  = errFace
  in sumTri ps

getNormalToFace :: VoronoiGrain Point3D -> VoronoiFace Point3D -> Vec3
getNormalToFace g f = normalize $ (faceTo f) &- (grainCenter g)

getCentroid :: [Vec3] -> Vec3
getCentroid [] = zero
getCentroid xt@(x:xs) = (foldl' (&+) x xs) &* k
  where k = 1 / (fromIntegral $ length xt)
        
-- TODO change better name or create a class
getFaceAreaFracHist :: (PointND a)=> [VoronoiFace a] -> [Double]
getFaceAreaFracHist = map (getArea.getFaceArea)

getTotalGrainVolume :: [VoronoiGrain Point3D] -> Volume
getTotalGrainVolume = foldl' (\acc x -> (getGrainVolume x) + acc) 0

getTotalGrainArea :: [VoronoiGrain Point3D] -> Area
getTotalGrainArea = foldl' (\acc x -> (getGrainArea x) + acc) 0

getAverageGrainVolume :: [VoronoiGrain Point3D] -> Volume
getAverageGrainVolume [] = 0
getAverageGrainVolume gs = (getTotalGrainVolume gs) / (Volume $ fromIntegral $ length gs)

getStdDeviationGrainVolume :: [VoronoiGrain Point3D] -> Volume
getStdDeviationGrainVolume [] = 0
getStdDeviationGrainVolume gs = totDev / n
  where
    totDev = foldl' (\acc x -> acc + (avg-(getGrainVolume x))^2) 0 gs
    avg = getAverageGrainVolume gs
    n = Volume $ fromIntegral $ length gs


