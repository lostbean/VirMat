{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment
import Text.Printf
import Control.Monad

import Math.DeUni
import Viewer3D
import VoronoiBuilder
import GrainQuery
import ShowData hiding (Box3D)
import GrainDistributionGenerator
import CommandLineInput
import OnionLayerControl

import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.List (foldl',sortBy,(\\),nub)
import Maybe
import Data.Vec hiding (length, map)

import Data.Random
import Data.Random.RVar
import System.CPUTime

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

main = do
    putStrLn ("VirMat v0.0.5 03/2011 [by Edgar Gomes]")
    putStrLn  "______________________________________"
    jobReq <- getArgs >>= (return.parseArgs)
    gen    <- getRandomGen $ seed jobReq
    let
      n    = targetNumber jobReq
      vol  = targetMeanVolume jobReq
      var  = targetVariance jobReq
      anis = anisotropyShapeRatio jobReq
    (DistributedPoints box ps) <- case distrType jobReq of
      FullDistribution  -> getFullRandomGrainDistribution gen n vol anis
      InBoxDistribution -> getGrainBoxDistribution gen n vol var anis
      OnionDistribution -> getOnionDistribution gen n vol var anis 10000000000

    let
        hull         = runDeHull box ps
        (wall,_)     = runDeWall box ps
        inBoxSimplex = onlySimpleInBox box wall
        grains       = convertDT2Voronoi inBoxSimplex

    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn $ "Box: " ++ show box
    putStrLn $ "Number of gen. points: "  ++ show (length ps)
    putStrLn $ "Number of gen. simplex: " ++ show (length inBoxSimplex)
    putStrLn $ "Number of gen. grains: "  ++ show (length grains)
    putStrLn $ "Average grain volume: " ++ (show.getVolume.getAverageGrainVolume) grains
    putStrLn $ "Std Deviation of grain volume: " ++ (show.getVolume.getStdDeviationGrainVolume) grains
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

    case outputFile jobReq of
      OutputFile file types -> do
        writeFile file ((getHeader jobReq) ++ (concatMap (printGrainInfo grains) (nub types)))
      _                     -> return ()


    let render t = map (render3DMC box ps wall) (nub t) in case showIn3D jobReq of
      ShowAll t    -> showMap (render t) box
      ShowOnly t i -> showMap (render t) box
      _            -> return ()


printGrainInfo::[VoronoiGrain] -> OutputInfoType -> String
printGrainInfo grains outType = 
  let printColumn f = (concatMap ((++ "\n").show.f) grains)
  in case outType of
    GrainVolume -> "\n\n# ==Grain volume==\n" ++ printColumn (getVolume.getVolumeGrain)
    GrainArea   -> "\n\n# ==Grain area==\n"   ++ printColumn (getArea.getAreaGrain)
    GrainNumber -> "\n\n# ==Grain number==\n"  ++ show (length grains)

getHeader::JobRequest -> String
getHeader job = "# Distribution Type: "        ++ (show $ distrType job)
             ++ "\n# Seed for PRNG: "          ++ (show $ seed job)
             ++ "\n# Target number of grain: " ++ (show $ targetNumber job)
             ++ "\n# Target mean volume: "     ++ (show $ targetMeanVolume job)
             ++ "\n# Target variance: "        ++ (show $ targetVariance job)
             ++ "\n# Aspect ratio:  "          ++ (show $ anisotropyShapeRatio job)
    
render3DMC::Box -> [Vec3D] -> [Simplex] -> Show3DType -> Renderable
render3DMC box ps wall showType =  case showType of  
  VoronoiGrain3D -> showGrain
  Box3D          -> showBox
  Hull3D         -> showHull
  Points3D       -> showPoints
  Simplex3D      -> showSimplex
  where
    showSimplex = packRender $ renderTetrahedron3D box wall
    showPoints  = packRender $ renderPoint3D 1 ps
    showBox     = packRender $ [renderBox3D box]
    showGrain   = packRender $ renderGrain3D box grains
    showHull    = packRender $ renderHullFaces hull  
    hull         = runDeHull box ps
    inBoxSimplex = onlySimpleInBox box wall
    grains       = convertDT2Voronoi inBoxSimplex


{-
getVolume::SetCellPoint -> Simplex -> Double
getVolume p simplex = abs (prismVolume / 6)
    where
        (a,b,c,d) = setCellID simplex
        prismVolume = (unpack ((p!a) - (p!d))) `dot` ((unpack (p!b - p!d)) `cross` (unpack (p!c - p!d)))
-}

onlySimpleInBox::Box -> [Simplex] -> [Simplex]
onlySimpleInBox box ls = filter ((isInBox box).circumSphereCenter) ls