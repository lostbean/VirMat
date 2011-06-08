-----------------------------------------------------------------------------
--
-- Module      :  OnionLayerControl
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Edgar Gomes
-- Stability   :  dev
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module OnionLayerControl
( getaPointInBox
, getaPointAtDist
, genSeedSimplex
, getaPointFromTriangle
, getaPointFromFace
, getOnionDistribution
) where


import Control.Monad (liftM)
import Data.Random
import Data.Random.RVar
import System.Random.Mersenne.Pure64
import Data.IORef
import Data.Vec hiding (fromList)
import Data.Array.IArray ((!))
import Data.Set hiding (filter)
import Monad
import Math.DeUni

import GrainDistributionGenerator (calcBoxSize, DistributedPoints(..))


getaPointInBox::IORef PureMT -> Box -> IO Vec3D
getaPointInBox gen (Box{xMax,xMin,yMax,yMin,zMax,zMin}) = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    a <- sampleFrom gen (uniform xMin xMax)
    b <- sampleFrom gen (uniform yMin yMax)
    c <- sampleFrom gen (uniform zMin zMax)
    return $ Vec3D a b c


getaPointAtDist::IORef PureMT -> RVar Double -> Vec3D -> IO Vec3D
getaPointAtDist gen f ref = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    dist <- sampleFrom gen f
    dx   <- sampleFrom gen stdUniform
    dy   <- sampleFrom gen stdUniform
    dz   <- sampleFrom gen stdUniform
    let norm = sqrt (dx^2 + dy^2 + dz^2)
        k = dist/norm
        displace = Vec3D (dx*k) (dy*k) (dz*k)
    return (ref + displace)


genSeedSimplex::IORef PureMT -> RVar Double -> Box -> IO [Vec3D]
genSeedSimplex gen var box = do
    a <- getaPointInBox gen box
    b <- getaPointAtDist gen var a
    let  medAB = (a + b)/2
    c <- getaPointAtDist gen var medAB
    let n = normalize $ pack $ (unpack (b - a)) `cross` (unpack (c - a))
    d <- getaPointFromTriangle gen var (a, b, c) n
    return [a, b, c, d]


getaPointFromTriangle::IORef PureMT -> RVar Double -> (Vec3D, Vec3D, Vec3D) -> Vec3D -> IO Vec3D
getaPointFromTriangle gen var (pa, pb, pc) nd = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    dist <- sampleFrom gen var
    let
        centroidFace = (pa + pb + pc)/3
        displace     = nd * ((pack.vec.abs) dist)
    return (centroidFace + displace)



getaPointFromFace::IORef PureMT -> RVar Double -> Face -> IO Vec3D
getaPointFromFace gen var face = do
    let (pa, pb, pc) = facePoints face
    getaPointFromTriangle gen var (pa, pb, pc) (refND face)


genOnion::IORef PureMT -> RVar Double -> Box -> IO [Vec3D]
genOnion gen var box = do
    seed <- genSeedSimplex gen var box
    print box
    print seed
    print "Gen..."
    fillBox [] seed
    where
        fillBox _ []     = return []
        fillBox oldPS ps = do
            let hull = runDeHull box (oldPS ++ ps)
            (newP, oldSetP) <- foldM (checkHullPoints gen var box) ([], empty) hull
            liftM (newP ++) (fillBox (elems oldSetP) newP)

        checkHullPoints gen var box (newPs, setOldP) face = do
          p <- getaPointFromFace gen var face
          if isInBox box p then return (p:newPs, setOldP) else return (newPs, distributeFace face setOldP)
          where distributeFace face set = let (a,b,c) = facePoints face in insert a $ insert b $ insert c set

genOnion'::IORef PureMT -> RVar Double -> Box -> Int -> IO ([Vec3D], [Vec3D])
genOnion' gen var box count = do
    seed <- genSeedSimplex gen var box
    fillBox [] seed count
    where
        fillBox acc ps 0 = return (acc, ps)
        fillBox acc [] _ = return (acc, [])
        fillBox acc ps i = do
            let psAll = ps ++ acc
                hull  = runDeHull box psAll
            newPS <- mapM (getaPointFromFace gen var) hull
            let inBoxPS = filter (isInBox box) newPS
            fillBox psAll inBoxPS (i-1)




getOnionDistribution::IORef PureMT -> Int -> Double -> Double -> (Double, Double, Double) -> Int -> IO DistributedPoints
getOnionDistribution gen targetNGrains avgVolume stdDev ratio n = do
  print box
  (ps,_) <- genOnion' gen (normal avgVolume stdDev) box n
  return $ DistributedPoints box ps 
  where
    box         = calcBoxSize totalVolume ratio
    totalVolume = avgVolume*(fromIntegral targetNGrains)
















