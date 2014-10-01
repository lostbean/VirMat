{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VirMat.Distributions.GrainSize.GrainQuery
  ( getGrainVolume
  , getVoronoiFaceArea
  , getCentroid
  , getAverage
  , getStdDeviation
  , Volume (getVolume)
  , Area   (getArea)
  , Length (getLength)
  , getFaceAreaFracHist
  ) where

import qualified Data.Vector  as V
import qualified Data.HashSet as HS

import           Data.Vector   (Vector)
import           Data.HashSet  (HashSet)
import           Data.List     (foldl')

import           DeUni.Types
import           Hammer.Math.Algebra
import           Hammer.MicroGraph

import           VirMat.Core.VoronoiMicro


newtype Length = Length
  { getLength :: Double
  } deriving (Eq, Ord, Num, Fractional, Show)

newtype Area = Area
  { getArea :: Double
  } deriving (Eq, Ord, Num, Fractional, Show)

newtype Volume = Volume
  { getVolume :: Double
  } deriving (Eq, Ord, Num, Fractional, Show)

-- TODO change better name or create a class
getFaceAreaFracHist :: MicroGraph a b c d -> [Double]
getFaceAreaFracHist = map getArea . getFaceAreas

getFaceAreas m = let
  gs = getGrainIDList m
  as = map (V.fromList . getVoronoiVertex m . getVoronoiEdges m)
  in undefined

getVoronoiEdges :: MicroGraph a b c d -> GrainID -> HashSet EdgeID
getVoronoiEdges m gid = let
  func = HS.foldl' (\es f -> maybe es (HS.union es) (getFaceProp f m >>= getPropConn)) HS.empty
  in maybe HS.empty func (getGrainProp gid m >>= getPropConn)

getVoronoiVertex :: MicroGraph a b c v -> HashSet EdgeID -> [v]
getVoronoiVertex m es = let
  vs = HS.foldl' (\vs e -> maybe vs (HS.union vs . func) (getEdgeProp e m >>= getPropConn)) HS.empty es
  func (FullEdge v1 v2) = HS.fromList [v1, v2]
  func _                = HS.empty
  in HS.foldl' (\xs v -> maybe xs (:xs) (getVertexProp v m >>= getPropValue)) [] vs

getGrainVolume :: (AbelianGroup a, MultiVec a, DotProd a, CrossProd a)
               => Vector (Vector a) -> (Volume, a)
getGrainVolume grain = let
  facesCenters  = V.map getCentroid grain
  grainCenter   = getCentroid facesCenters
  volumes       = V.zipWith prismVolume facesCenters grain
  volume        = V.sum volumes
  prismVolume fcenter f = let
    (area, _) = getVoronoiFaceArea f
    normal    = getNormalToFace grainCenter f
    in (normal &. fcenter) * (getArea area)
  in  (Volume $ abs (volume / 3), grainCenter)


triangleNormal :: (AbelianGroup a, DotProd a, CrossProd a, MultiVec a)=> a -> a -> a -> a
triangleNormal a b c = let
  ca = a &- c
  cb = b &- c
  in normalize $ ca &^ cb

triangleArea :: (AbelianGroup a, DotProd a)=> a -> a -> a -> Area
triangleArea a b c = let
  ca = a &- c
  cb = b &- c
  dot = ca &. cb
  x   = normsqr cb * normsqr ca - dot * dot
  in Area (0.5 * sqrt x)

triangleVolume :: (AbelianGroup a, DotProd a, CrossProd a)=> a -> a -> a -> Area
triangleVolume a b c = Area $ (a &. (b &^ c)) / 6

-- | Calculate the area of each face using a signed sum of triangles.
-- It assumes that the edge pairs are given in a sorted sequnce,
-- e.g. (P0,P1,P2 .... Pn)
getVoronoiFaceArea :: (AbelianGroup a, DotProd a, MultiVec a)
                   => Vector a -> (Area, a)
getVoronoiFaceArea face = let
  center = getCentroid face
  stop = V.length face - 1
  sumTri n
    | n >= stop = 0
    | otherwise = let
      a = face V.! n
      b = face V.! (n + 1)
      in sumTri (n + 1) + triangleArea a b center
  in (sumTri 1, center)

getNormalToFace :: (AbelianGroup a, CrossProd a, DotProd a, MultiVec a)
                => a -> Vector a -> a
getNormalToFace center face
  | V.length face <= 2 = zero
  | dir           >  0 = neg n
  | otherwise          = n
  where
    a = face V.! 0
    b = face V.! 1
    c = face V.! 2
    n = normalize $ (a &- b) &^ (c &- b)
    dir = n &. (center &- a)

getCentroid :: (AbelianGroup a, MultiVec a)=> Vector a -> a
getCentroid v = (V.foldl' (&+) zero v) &* k
  where k = 1 / (fromIntegral $ V.length v)

getAverage :: (Num a, Fractional a)=> [a] -> a
getAverage gs = let
  (total, n) = foldl' (\(vacc, nacc) x -> (vacc + x, nacc + 1)) (0, 0) gs
  in total / n

getStdDeviation :: (Num a, Fractional a)=> [a] -> a
getStdDeviation [] = 0
getStdDeviation gs = totDev / n
  where
    totDev = foldl' (\acc x -> acc + (avg - x) * (avg - x)) 0 gs
    avg    = getAverage gs
    n      = fromIntegral $ length gs
