{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE  RecordWildCards #-}

module VirMat.Run3D where

import VirMat.Core.VoronoiBuilder
import VirMat.Core.Packer
import VirMat.Distributions.GrainSize.StatTools
import VirMat.Distributions.GrainSize.GrainDistributionGenerator
import VirMat.Distributions.GrainSize.GrainQuery
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


onlyDistInBox::(PointND a)=> Box a -> SetPoint a -> SetPoint a
onlyDistInBox box sp = Vec.filter ((isInBox box).point) sp

onlySimpleInBox::Box Point3D -> IM.IntMap (S2 Point3D) -> IM.IntMap (S2 Point3D)
onlySimpleInBox box ls = IM.filter ((isInBox box).circumSphereCenter) ls

