{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE  RecordWildCards #-}

module Main where

import VirMat.Core.VoronoiBuilder
import VirMat.Core.Packer
import VirMat.Distributions.GrainSize.StatTools
import VirMat.Distributions.GrainSize.GrainDistributionGenerator
import VirMat.Distributions.GrainSize.GrainQuery
import VirMat.Distributions.Texture.ODFSampling
import VirMat.IO.Export.VTK.VTKVoronoiGrainRender
import VirMat.IO.Import.CommandLineInput
import VirMat.IO.Import.Types
import VirMat.IO.Export.SVG.RenderSVG

import Control.Monad
import System.Environment (getArgs)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Data.List (foldl',sortBy,(\\),nub)
import Data.Maybe (Maybe, isJust)
import Data.Monoid ((<>))
import Data.Random
import Data.Random.RVar
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import Diagrams.Prelude ((===), (|||), scaleX)

import DeUni.DeWall
import DeUni.Types
import DeUni.Dim3.Base3D
import DeUni.Dim2.Base2D
import Hammer.Math.Vector hiding (Vector)
import qualified Hammer.Math.Vector as AlgLin

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

main = do
    putStrLn ("VirMat v0.3 03/2012 [by Edgar Gomes]")
    putStrLn  "____________________________________"
    jobReq <- getArgs >>= (return.parseArgs)
    gen    <- getRandomGen $ seed jobReq
    let
      n    = targetNumber jobReq
      (gsFunc, gsMean) = composeDist $ gsDist jobReq
      
-- ========================== 3D ===================================
{--    
    pdist@(DistributedPoints box arr) <- case distrType jobReq of
      RandomDistribution -> genFullRandomGrainDistribution' gen n vol (composeDist gsFunc, (0, 100)) anis
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
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    
    writeVoronoiGrainVTKfile "test.vtu" wall grains
--}   
 
-- ==================================== 2D ============================================= 
    pdist2D@(DistributedPoints box2D arr0) <- case distrType jobReq of
      RandomDistribution -> genFullRandomGrainDistribution' gen n (pi * gsMean) (gsFunc, (0, 100)) (1.0,1.0)
      PackedDistribution -> undefined
    
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn $ "Box: " ++ show box2D
    putStrLn $ "Average grain volume: " ++ show (pi * gsMean)
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    --print arr0
    
    let
      len0             = Vec.length arr0
      pset0            = [0..len0-1]
      (wall0,wallSt0) = runDelaunay2D box2D arr0 pset0
      grains0          = convertDT2Voronoi arr0 (onlySimpleInBox2D box2D wall0)
    
    printMicro "Initial" box2D arr0 wall0 grains0
    printGrainHist "Initial" grains0 arr0
      
    let
      pack (!arr0,!arr1,!wall1) i = let
        time      = if i > 12 then 0.08 else 0.06
        arr2      = updateSP box2D wall1 arr0 arr1 0.6 time
        len2      = Vec.length arr2
        pset2     = [0 .. len2-1]
        (wall2,_) = if i > 60 then (wall1, undefined) else runDelaunay2D box2D arr2 pset2
        grains2 = convertDT2Voronoi arr2 (onlySimpleInBox2D box2D wall2)
        in do
          --printEvo i box2D arr2 wall2 grains2          
          return (arr1,arr2,wall2)
    
    (arrF0, arrF1, wallF1)<- foldM pack (arr0,arr0,wall0) [1..60::Int]
    
    let grainsF1 = convertDT2Voronoi arrF1 (onlySimpleInBox2D box2D wallF1)
    printMicro "Final" box2D arrF1 wallF1 grainsF1
    printGrainHist "Final" grainsF1 arrF1



printEvo id box sp wall grains = let
  name = let
    n = show id
    l = length n
    zeros = take (3 - l) ['0', '0' ..]
    in zeros ++ n
  forces = setForce wall sp
  
  diaUP1 = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D sp
        <> renderForces sp forces
  
  diaUP2 = closeUpOnBox box $
           renderBox2D box
        <> renderSetGrain2D grains

  histFinal  = freqHist 0 200 5 (getFaceAreaFracHist grains)
  histTarget = freqHist 0 200 5 (map ((* pi).weigth) $ Vec.toList sp)
  diaFinal  = renderHistogram' 0 200 (map (*1000) histFinal)
  diaTarget = renderHistogram' 0 200 (map (*1000) histTarget)
  in renderSVGFile ("evoluton_" ++ name ++ ".svg") (sizeSpec (Just 1000, Just 1000)) $ (diaUP1 ||| diaUP2) === (diaTarget ||| diaFinal)

printMicro name box sp wall grains = let
  forces = setForce wall sp
  
  diaUP1 = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D sp
        <> renderForces sp forces
  
  diaUP2 = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D sp
        <> renderSetS2Triangle sp wall
  
  diaUP3 = closeUpOnBox box $
           renderBox2D box
        <> renderSetGrain2D grains

  in renderSVGFile ("microstructure_" ++ name ++ ".svg") (sizeSpec (Just 500, Just 1500)) $ diaUP1 === diaUP2 === diaUP3 

printGrainHist name grains sp = let
  histFinal  = freqHist 0 200 2 (getFaceAreaFracHist grains)
  histTarget = freqHist 0 200 2 (map ((* pi).weigth) $ Vec.toList sp)
  diaFinal  = renderHistogram' 0 200 (map (*1000) histFinal)
  diaTarget = renderHistogram' 0 200 (map (*1000) histTarget)
  in renderSVGFile ("hist_" ++ name ++ ".svg") (sizeSpec (Just 500, Just 1000)) $ diaTarget === diaFinal    


onlyDistInBox::(PointND a)=> Box a -> SetPoint a -> SetPoint a
onlyDistInBox box sp = Vec.filter ((isInBox box).point) sp

onlySimpleInBox2D::Box Point2D -> IM.IntMap (S2 Point2D) -> IM.IntMap (S2 Point2D)
onlySimpleInBox2D box ls = IM.filter ((isInBox box).circleCenter) ls

onlySimpleInBox::Box Point3D -> IM.IntMap (S2 Point3D) -> IM.IntMap (S2 Point3D)
onlySimpleInBox box ls = IM.filter ((isInBox box).circumSphereCenter) ls

