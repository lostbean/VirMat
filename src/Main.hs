-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  LGPL
--
-- Maintainer  :
-- Stability   :  No
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}


module Main where


import System.Environment
import Text.Printf
import System.Random.Mersenne
import Control.Monad

--import Delauny
import DelaunayReverseOnion
import Viewer3D
import VoronoiCreator
import GrainQuery
import ShowData
import GrainDistributionGenerator
import CommandLineInput

import Data.Vec hiding (map, length, take)
import Data.Array.IArray (listArray, indices)
import qualified Data.Set as Set
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Maybe (isJust)
import Data.List(foldl')

import Control.Monad.State


import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

main = do
    putStrLn ("VirMat v0.0.1 03/2010 \n--by Edgar Gomes")
    putStrLn "----------------------------------------"
    (grainDistIn, fileOut, showIn3D) <- getArgs >>= (return.parseArgs)
    gen <- getRandomGen $ seed grainDistIn
    --(DistributedPoints box zee) <- getGrainBoxDistribution gen (targetNumber grainDistIn) (targetMeanVolume grainDistIn) (targetVariance grainDistIn) (anisotropyShapeRatio grainDistIn)
    (DistributedPoints box zee) <- getFullRandomGrainDistribution gen (targetNumber grainDistIn) (targetMeanVolume grainDistIn) (anisotropyShapeRatio grainDistIn)
    --let zee = [ Vec3D 1.2567592328085344 2.1538398397311407 1.3803230051305468, Vec3D 0.345506130699831 1.062270175116883 0.42663513582995694, Vec3D 2.199387164040813 1.70709030167904 1.179053056943591, Vec3D 1.012625043754363 1.2977297474651666 1.0561425883593771, Vec3D 0.8833130253370662 2.084918120283064 2.044543792610099, Vec3D 1.3526887964252738  0.6180048571782097 2.1820258944756334, Vec3D 0.8578790193449071 1.2441928333014776 2.1694683864 ]
    --let box = DelaunayReverseOnion.Box {xMax = 3.0, xMin = 0.0, yMax = 3.0, yMin = 0.0, zMax = 3.0, zMin = 0.0}
    let
        !points = debug "Points" $ listArray (1,SimplexPointer $ length zee) zee

        ref = indices points
        -- Distribution in getting points out of the box
        --box = getBox points ref
        wall = execState (deWall ref Set.empty box) (initDeWallState points)
        de = (sigmas wall) --onlySimpleInBox box (sigmas wall)

        cell = convert points de ref
        inBoxCell = map (\(Just x) -> x) $ filter isJust cell

        vertexArray = getVertexPointArray de
        -- !vertexData = make3DData (points, de)
        !vertexDataG = make3DDataG box (getVertexPointArray de) inBoxCell

        showOpenGL = case showIn3D of
            (ShowOnly n) -> showMap (take n vertexDataG) box
            All -> showMap vertexDataG box

        area = map (getAreaGrain vertexArray) inBoxCell
        volume = map (getVolumeGrain vertexArray) inBoxCell
        totalVolume = foldl' (\a b -> a + getVolume b) 0 volume
        totalArea = foldl' (\a b -> a + getArea b) 0 area
        nCell = length volume
        meanVol = totalVolume/(fromIntegral nCell)
        devTotal = foldl' (\a b -> (a + (meanVol - getVolume b)^2)) 0 volume
        devVol = devTotal/(fromIntegral nCell)

    putStrLn $ "Box: " ++ show box
    putStrLn $ "Volume: " ++ (show $ volume )
    putStrLn $ "\n - mean: " ++ show meanVol ++ "\n - dev: " ++ show devVol
    putStrLn $ "N rand points: " ++ show (length ref)
    putStrLn $ "N simplex: " ++ show (length de)
    putStrLn $ "n Clean Cell :" ++ show nCell
    putStrLn $ "TotalVolume: " ++ (show $ totalVolume )
    putStrLn $ "Box: " ++ (show $ map setSimplexPointer de)


    writeFile (grainDistroOutFile fileOut) (concatMap (\(i,v,a) -> show i ++ "\t" ++ (show.getVolume) v ++ "\t" ++ (show.getArea) a ++ "\n") $ zip3 [1..] volume area)


    showOpenGL




