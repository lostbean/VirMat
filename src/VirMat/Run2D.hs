{-# LANGUAGE RecordWildCards #-}
module VirMat.Run2D
  ( runVirMat2D
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

-- ============================== 2D =====================================

samplePoints :: JobRequest -> IO (DistributedPoints Vec2)
samplePoints VoronoiJob{..} = case structSize of
  NGrains n -> genGrainDistributionByNumber (1, 1) seed gsDist n
  SizeBox (x, y, _) -> let
    box = Box2D {xMax2D = x, xMin2D = 0, yMax2D = y, yMin2D = 0}
    in genGrainDistributionByBox seed gsDist box

runVirMat2D :: JobRequest -> IO (Simulation Vec2)
runVirMat2D job = buildMicro <$> genPoints job

genPoints :: JobRequest -> IO (DistributedPoints Vec2)
genPoints jobReq = case distrType jobReq of
  RandomDistribution       -> samplePoints jobReq
  PackedDistribution nstep -> do
    (DistributedPoints box2D ps0) <- samplePoints jobReq
    let
      len0         = V.length ps0
      psID0        = [0..len0-1]
      (wall0, _)   = runDelaunay2D box2D ps0 psID0
      (psFinal, _) = runPacker nstep box2D ps0 wall0
    return $ DistributedPoints box2D psFinal

buildMicro :: DistributedPoints Vec2 -> Simulation Vec2
buildMicro (DistributedPoints box2D ps) = let
  ls     = V.length ps
  psID   = [0 .. ls - 1]
  wall   = fst $ runDelaunay2D box2D ps psID
  grains = mkVoronoiMicro (onlySimpleInBox2D box2D wall)
  in Simulation { box           = box2D
                , pointSet      = ps
                , triangulation = wall
                , grainSet      = grains }

onlySimpleInBox2D :: Box Vec2 -> IM.IntMap (S2 Vec2) -> IM.IntMap (S2 Vec2)
onlySimpleInBox2D box = IM.filter (isInBox box . circleCenter)
