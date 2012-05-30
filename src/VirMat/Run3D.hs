{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE  RecordWildCards #-}

module VirMat.Run3D where

import VirMat.Types
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
import Hammer.Math.Vector hiding (Vector)
import qualified Hammer.Math.Vector as AlgLin

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x


-- ==================================== 3D =============================================
runVirMat3D::JobRequest -> IO (Simulation Point3D)
runVirMat3D jobReq = do
    pdist2D@(DistributedPoints box3D ps0) <- case distrType jobReq of
        RandomDistribution -> genFullRandomGrainDistribution (1, 1, 1) jobReq
        PackedDistribution -> undefined
    -- DEBUG
    let Just mdist = composeDist . gsDist $ jobReq
    print $ mDistMean mdist
    print $ mDistInterval mdist
    print $ mDistModes mdist
    print $ mDistArea mdist
    --print ps0
    -- =====
    let
      len0                 = Vec.length ps0
      psID0                = [0..len0-1]
      (wall0,wallSt0)      = runDelaunay3D box3D ps0 psID0

      (psFinal, wallFinal) = (ps0, wall0) --runPacker 60 box2D ps0 wall0
      grainsFinal          = convertDT2Voronoi psFinal (onlySimplexInBox3D box3D wallFinal)

    return $ Simulation { box = box3D, pointSet = psFinal, triangulation = wallFinal, grainSet = grainsFinal }


onlyDistInBox::(PointND a)=> Box a -> SetPoint a -> SetPoint a
onlyDistInBox box sp = Vec.filter ((isInBox box).point) sp

onlySimplexInBox3D::Box Point3D -> IM.IntMap (S2 Point3D) -> IM.IntMap (S2 Point3D)
onlySimplexInBox3D box ls = IM.filter ((isInBox box).circumSphereCenter) ls

