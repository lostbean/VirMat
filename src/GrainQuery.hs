{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module GrainQuery
( getAreaGrain
, getVolumeGrain
, getTotalGrainVolume
, getAverageGrainVolume
, getStdDeviationGrainVolume
, getTotalGrainArea
, Volume (getVolume)
, Area   (getArea)
) where

import VoronoiBuilder
import Data.Vec hiding (length, map, foldl)
import Math.DeUni (Simplex(..))
import Data.List (foldl')

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = x --trace (s ++ show x) x

newtype Area   = Area   {getArea::Double}   deriving (Eq, Ord, Num, Fractional, Show)
newtype Volume = Volume {getVolume::Double} deriving (Eq, Ord, Num, Fractional, Show)

getVolumeGrain::VoronoiGrain -> Volume
getVolumeGrain grain = Volume $ abs (volume / 3)
    where
        volume        = foldl' (\a b -> a + (prismVolume b)) 0 (faces grain)
        prismVolume f = (normal `dot` centroid) * (getArea area)
          where area   = getFaceArea normal f
                normal = getNormalToFace grain f
                centroid = getCentroid (map (circumSphereCenter.snd) (edges f))

getAreaGrain::VoronoiGrain -> Area
getAreaGrain grain = foldl' (\a b -> a + (getArea b)) (Area 0) (faces grain)
  where getArea x  = getFaceArea (getNormalToFace grain x) x

-- | Calculate the area of each face using a signed sum of triangles.
-- It assume that the edge pairs are given in a oriented sequnce, e.g. (P0,P1,P2 .... Pn)
-- $A(\Delta)=\dfrac{1}{2}(v_{1}-v_{0})\times(v_{2}-v_{0})$
-- $A(\Omega)=\dfrac{1}{2}n\cdot{\displaystyle \sum_{i=0}^{i-1}}(v_{i}\times v_{i+1})$ 
getFaceArea::Vec3D -> VoronoiFace -> Area
getFaceArea normal face = debug ("area: " ++ show face ++ " :--> ") area
    where
        area    = Area $ abs $ 0.5 * (normal `dot` (sumTri vs))
        vs      = map (circumSphereCenter.snd) (edges face)
        errFace = error "Error: Malformed face - trying to define a face with less than 3 points."
        -- the result will not make sense if the list conteins less than 3 points
        sumTri::[Vec3D] -> Vec3D
        sumTri []       = errFace
        sumTri [_,_]    = errFace
        sumTri xt@(a:_) = (thunkSum xt) a
          where
          thunkSum::[Vec3D] -> (Vec3D -> Vec3D)  
          thunkSum (a1:a2:as) = ((pack $ (unpack a1) `cross` (unpack a2)) +).(thunkSum (a2:as))
          -- boundary condition for a non empty list set
          thunkSum [a]        = \x -> pack $ (unpack a) `cross` (unpack x)
          thunkSum []         = errFace

getNormalToFace::VoronoiGrain -> VoronoiFace -> Vec3D
getNormalToFace g f = normalize $ (faceTo f) - (grainCenter g)

getCentroid::[Vec3D] -> Vec3D
getCentroid [] = Vec3D 0 0 0
getCentroid xt@(x:xs) = (foldl' (+) x xs) / (Vec3D k k k)
  where k = fromIntegral $ length xt
        
getTotalGrainVolume::[VoronoiGrain] -> Volume
getTotalGrainVolume = foldl' (\acc x -> (getVolumeGrain x) + acc) 0

getTotalGrainArea::[VoronoiGrain] -> Area
getTotalGrainArea = foldl' (\acc x -> (getAreaGrain x) + acc) 0

getAverageGrainVolume::[VoronoiGrain] -> Volume
getAverageGrainVolume [] = 0
getAverageGrainVolume gs = (getTotalGrainVolume gs) / (Volume $ fromIntegral $ length gs)

getStdDeviationGrainVolume::[VoronoiGrain] -> Volume
getStdDeviationGrainVolume [] = 0
getStdDeviationGrainVolume gs = totDev / n
  where
    totDev = foldl' (\acc x -> acc + (avg-(getVolumeGrain x))^2) 0 gs
    avg = getAverageGrainVolume gs
    n = Volume $ fromIntegral $ length gs


