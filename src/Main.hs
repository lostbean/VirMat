{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Core.IMC
import Core.VoronoiBuilder
import Distributions.GrainSize.DelaunayStatistics
import Distributions.GrainSize.GrainDistributionGenerator
import Distributions.GrainSize.GrainQuery
import Distributions.Texture.ODFSampling
import Douane.Export.VTK.VTKGenTetra
import Douane.Export.Viewer.ShowData hiding (Box3D, SetPoint)
import Douane.Export.Viewer.Viewer3D
import Douane.Import.Prompt.CommandLineInput
import Math.DeUni
import qualified Distributions.GrainSize.DelaunayStatistics as DS

import Control.Monad
import Data.Array.Diff (listArray, (!), elems, bounds, (//), DiffArray)
import Data.List (foldl',sortBy,(\\),nub)
import Data.Maybe (isJust)
import Data.Random
import Data.Random.RVar
import Data.Vec (Vec3D)
import Maybe
import System.CPUTime
import System.Environment
import Text.Printf
import qualified Data.IntMap as IM
import qualified Data.Map as Map

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
    pdist@(DistributedPoints box arr) <- case distrType jobReq of
      FullDistribution  -> getFullRandomGrainDistribution gen n vol anis
      InBoxDistribution -> getGrainBoxDistribution gen n vol var anis
      --OnionDistribution -> getOnionDistribution gen n vol var anis 10000000000

    let
      (lb,hb)       = bounds arr
      pset          = [lb..hb]
      hull          = runDeHull box arr pset
      (wall,wallSt) = runDeWall box arr pset
      inBoxSimplex  = onlySimpleInBox box wall
      grains        = convertDT2Voronoi arr inBoxSimplex

    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn $ "Box: " ++ show box
    putStrLn $ "Number of gen. points: "  ++ show (hb-lb+1)
    putStrLn $ "Number of gen. simplex: " ++ show (IM.size wall)
    putStrLn $ "Number of gen. grains: "  ++ show (length grains)
    putStrLn $ "Average grain volume: " ++ (show.getVolume.getAverageGrainVolume) grains
    putStrLn $ "Std Deviation of grain volume: " ++ (show.getVolume.getStdDeviationGrainVolume) grains
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
            
    writeVTKfile "micro.vtu" grains
    let 
      binorm x = DS.normal 1.5 0.5 x --(DelaunayStatistics.normal 1.5 0.5 x) + (DelaunayStatistics.normal 5 0.7 x)
      targetdist = makeDistFunc binorm (0, 20)
    printTargetToFile "stat_target.txt" targetdist   
    
    (newWall, newSt) <- imc pdist gen targetdist
    let newArr = Math.DeUni.setPoint newSt
        newGrains = onlySimpleInBox box newWall
    
    testeIMCODF (findGSList newArr newWall)
    writeVTKfile "finalMicro.vtu" (convertDT2Voronoi newArr newGrains)    
    
    case outputFile jobReq of
      OutputFile file types -> do
        writeFile file ((getHeader jobReq) ++ (concatMap (printGrainInfo grains) (nub types)))
      _                     -> return ()


    let render t = map (render3DMC newArr box (elems newArr) newWall) (nub t) in case showIn3D jobReq of
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
    
render3DMC::SetPoint -> Box -> [Vec3D] -> IM.IntMap Simplex -> Show3DType -> Renderable
render3DMC arr box ps wall showType =  case showType of  
  VoronoiGrain3D -> showGrain
  Box3D          -> showBox
  Hull3D         -> showHull
  Points3D       -> showPoints
  Simplex3D      -> showSimplex
  where
    showSimplex = packRender $ renderTetrahedron3D arr box (IM.elems wall)
    showPoints  = packRender $ renderPoint3D 1 ps
    showBox     = packRender $ [renderBox3D box]
    showGrain   = packRender $ renderGrain3D box grains
    showHull    = packRender $ renderHullFaces arr (IM.elems hull) 
    (lbArr, hbArr) = bounds arr
    pSet = [lbArr..hbArr]
    (hull,_)     = runDeHull box arr pSet
    inBoxSimplex = onlySimpleInBox box wall
    grains       = convertDT2Voronoi arr inBoxSimplex


{-
getVolume::SetCellPoint -> Simplex -> Double
getVolume p simplex = abs (prismVolume / 6)
    where
        (a,b,c,d) = setCellID simplex
        prismVolume = (unpack ((p!a) - (p!d))) `dot` ((unpack (p!b - p!d)) `cross` (unpack (p!c - p!d)))
-}

onlySimpleInBox::Box -> (IM.IntMap Simplex) -> (IM.IntMap Simplex)
onlySimpleInBox box ls = IM.filter ((isInBox box).circumSphereCenter) ls