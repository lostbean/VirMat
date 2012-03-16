{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Core.VoronoiBuilder
import Distributions.GrainSize.DelaunayStatistics
import Distributions.GrainSize.GrainDistributionGenerator
import Distributions.GrainSize.GrainQuery
import Distributions.Texture.ODFSampling
import IO.Export.VTK.VTKVoronoiGrainRender
import IO.Import.CommandLineInput

import Control.Monad
import System.Environment (getArgs)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Data.List (foldl',sortBy,(\\),nub)
import Data.Maybe (Maybe, isJust)
import Data.Random
import Data.Random.RVar
import qualified Data.IntMap as IM
import qualified Data.Map as Map

import DeUni.DeWall
import DeUni.Types
import DeUni.Dim3.Base3D
import Hammer.Math.Vector hiding (Vector)

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

main = do
    putStrLn ("VirMat v0.3 03/2012 [by Edgar Gomes]")
    putStrLn  "______________________________________"
    jobReq <- getArgs >>= (return.parseArgs)
    gen    <- getRandomGen $ seed jobReq
    let
      n    = targetNumber jobReq
      vol  = targetMeanVolume jobReq
      var  = targetVariance jobReq
      anis = anisotropyShapeRatio jobReq
    pdist@(DistributedPoints box arr) <- case distrType jobReq of
      RandomDistribution -> genFullRandomGrainDistribution gen n vol (Data.Random.normal 15 1) anis
      PackedDistribution -> undefined


    let
      len           = Vec.length arr
      pset          = [0..len-1]
      hull          = runHull3D box arr pset
      (wall,wallSt) = runDelaunay3D box arr pset
      inBoxSimplex  = onlySimpleInBox box wall
      grains        = convertDT2Voronoi arr inBoxSimplex

    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn $ "Box: " ++ show box
    putStrLn $ "Number of gen. points: "  ++ show len
    putStrLn $ "Number of gen. simplex: " ++ show (IM.size wall)
    putStrLn $ "Number of gen. grains: "  ++ show (length grains)
    putStrLn $ "Average grain volume: "   ++ (show.getVolume.getAverageGrainVolume) grains
    putStrLn $ "Std Deviation of grain volume: " ++ (show.getVolume.getStdDeviationGrainVolume) grains
    --putStrLn $ "faces " ++ show (map (length . faces) grains)
    --putStrLn $ "edges " ++ show (map (map (map fst . edges) . faces) grains)
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    
    writeVoronoiGrainVTKfile "test.vtu" wall grains

onlySimpleInBox::Box Point3D -> IM.IntMap (S2 Point3D) -> IM.IntMap (S2 Point3D)
onlySimpleInBox box ls = IM.filter ((isInBox box).circumSphereCenter) ls
