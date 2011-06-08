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
import GrainQuery hiding (getVolume)
import ShowData
import GrainDistributionGenerator
import CommandLineInput

import Data.Vec hiding (map, length, take)
import Data.Array.IArray ((!),listArray, indices)
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
    (DistributedPoints box zee) <- getGrainBoxDistribution gen (targetNumber grainDistIn) (targetMeanVolume grainDistIn) (targetVariance grainDistIn) (anisotropyShapeRatio grainDistIn)
    --(DistributedPoints box2 zee) <- getFullRandomGrainDistribution gen (targetNumber grainDistIn) (targetMeanVolume grainDistIn) (anisotropyShapeRatio grainDistIn)
    let
        !points = listArray (1,SimplexPointer $ length zee) zee

        ref = indices points
        -- Distribution in getting points out of the box
        --box = getBox points ref
        wall = evalState (deWall ref Set.empty box) (initDeWallState points)
        de = onlySimpleInBox box wall

        cell = convert points de ref
        inBoxCell = map (\(Just x) -> x) $ filter isJust cell
{-
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

    --putStrLn $ "Box: " ++ show box
    --putStrLn $ "\n - mean: " ++ show meanVol ++ "\n - dev: " ++ show devVol
    --putStrLn $ "n Clean Cell :" ++ show nCell
-}    
    
    putStrLn $ "N simplex: " ++ show (length wall)
    --putStrLn $ "TotalVolume: " ++ (show $ totalVolume )
    --putStrLn $ "ID: " ++ (show $ finalID)
    let vol = foldl' (\a b -> a + Main.getVolume points b) 0 wall
    putStrLn $ "total simplex vol.: " ++ (show $ vol)
    --writeFile (grainDistroOutFile fileOut) showG


    --writeFile (grainDistroOutFile fileOut) (concatMap (\(i,v,a) -> show i ++ "\t" ++ (show.getVolume) v ++ "\t" ++ (show.getArea) a ++ "\n") $ zip3 [1..] volume area)


    --showOpenGL

getVolume::SetSimplexPoint -> Simplex -> Double
getVolume p simplex = abs (prismVolume / 6)
    where
        (a,b,c,d) = setSimplexPointer simplex
        prismVolume = (unpack ((p!a) - (p!d))) `dot` ((unpack (p!b - p!d)) `cross` (unpack (p!c - p!d)))


