{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Run3D
       ( runVirMat3D
       ) where

import           VirMat.Types
import           VirMat.Core.VoronoiMicro
import           VirMat.Core.Packer
import           VirMat.Distributions.GrainSize.GrainDistributionGenerator
import           VirMat.IO.Import.Types

import qualified Data.Vector as V
import qualified Data.IntMap as IM

import           DeUni.DeWall

--import Debug.Trace
--debug :: Show a => String -> a -> a
--debug s x = trace (s ++ show x) x

-- ============================== 3D ======================================

samplePoints :: JobRequest -> IO (DistributedPoints Point3D)
samplePoints VoronoiJob{..} = case structSize of
  NGrains n -> genGrainDistributionByNumber (1, 1, 1) seed gsDist n
  SizeBox (x, y, z) -> let
    box = Box3D {xMax3D = x, xMin3D = 0, yMax3D = y, yMin3D = 0, zMax3D = z, zMin3D = 0}
    in genGrainDistributionByBox seed gsDist box

runVirMat3D :: JobRequest -> IO (Simulation Point3D)
runVirMat3D job = genPoints job >>= return . buildMicro

genPoints :: JobRequest -> IO (DistributedPoints Point3D)
genPoints jobReq = case distrType jobReq of
  RandomDistribution       -> samplePoints jobReq
  PackedDistribution nstep ->  do
    (DistributedPoints box3D ps0) <- samplePoints jobReq
    let
      len0         = V.length ps0
      psID0        = [0..len0-1]
      (wall0, _)   = runDelaunay3D box3D ps0 psID0
      (psFinal, _) = runPacker nstep box3D ps0 wall0
    return $ DistributedPoints box3D psFinal


buildMicro :: DistributedPoints Point3D -> Simulation Point3D
buildMicro (DistributedPoints box3D ps) = let
  len       = V.length ps
  psID      = [0..len-1]
  (wall, _) = runDelaunay3D box3D ps psID
  grains    = mkVoronoiMicro (onlySimplexInBox3D box3D wall)
  in Simulation { box           = box3D
                , pointSet      = ps
                , triangulation = wall
                , grainSet      = grains }

onlySimplexInBox3D :: Box Point3D -> IM.IntMap (S2 Point3D) -> IM.IntMap (S2 Point3D)
onlySimplexInBox3D box = IM.filter (isInBox box . circumSphereCenter)

--onlyDistInBox :: (PointND a)=> Box a -> SetPoint a -> SetPoint a
--onlyDistInBox box = V.filter (isInBox box . point)
