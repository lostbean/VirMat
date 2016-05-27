{-# LANGUAGE RecordWildCards #-}
module VirMat.Run3D
  ( runVirMat3D
  ) where

import DeUni.DeWall
import Linear.Vect
import qualified Data.Vector as V
import qualified Data.IntMap as IM

import VirMat.Types
import VirMat.Core.VoronoiMicro
import VirMat.Core.Packer
import VirMat.Distributions.GrainSize.GrainDistributionGenerator
import VirMat.IO.Import.Types

-- ============================== 3D ======================================

samplePoints :: JobRequest -> IO (DistributedPoints Vec3)
samplePoints VoronoiJob{..} = case structSize of
  NGrains n -> genGrainDistributionByNumber (1, 1, 1) seed gsDist n
  SizeBox (x, y, z) -> let
    box = Box3D {xMax3D = x, xMin3D = 0, yMax3D = y, yMin3D = 0, zMax3D = z, zMin3D = 0}
    in genGrainDistributionByBox seed gsDist box

runVirMat3D :: JobRequest -> IO (Simulation Vec3)
runVirMat3D job = buildMicro <$> genPoints job

genPoints :: JobRequest -> IO (DistributedPoints Vec3)
genPoints jobReq = case distrType jobReq of
  RandomDistribution       -> samplePoints jobReq
  PackedDistribution nstep -> do
    (DistributedPoints box3D ps0) <- samplePoints jobReq
    let
      len0         = V.length ps0
      psID0        = [0..len0-1]
      (wall0, _)   = runDelaunay3D box3D ps0 psID0
      (psFinal, _) = runPacker nstep box3D ps0 wall0
    return $ DistributedPoints box3D psFinal


buildMicro :: DistributedPoints Vec3 -> Simulation Vec3
buildMicro (DistributedPoints box3D ps) = let
  len       = V.length ps
  psID      = [0..len-1]
  (wall, _) = runDelaunay3D box3D ps psID
  grains    = mkVoronoiMicro (onlySimplexInBox3D box3D wall)
  in Simulation { box           = box3D
                , pointSet      = ps
                , triangulation = wall
                , grainSet      = grains }

onlySimplexInBox3D :: Box Vec3 -> IM.IntMap (S2 Vec3) -> IM.IntMap (S2 Vec3)
onlySimplexInBox3D box = IM.filter (isInBox box . circumSphereCenter)
