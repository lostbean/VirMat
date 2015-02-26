{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Run2D
       ( runVirMat2D
       ) where

import qualified Data.Vector as V
import qualified Data.IntMap as IM

import           DeUni.DeWall

import           VirMat.Types
import           VirMat.Core.VoronoiMicro
import           VirMat.Core.Packer
import           VirMat.Distributions.GrainSize.GrainDistributionGenerator
import           VirMat.IO.Import.Types

--import           Debug.Trace
--debug  ::  Show a => String -> a -> a
--debug s x = trace (s ++ show x) x

-- ============================== 2D =====================================

samplePoints :: JobRequest -> IO (DistributedPoints Point2D)
samplePoints VoronoiJob{..} = case structSize of
  NGrains n -> genGrainDistributionByNumber (1, 1) seed gsDist n
  SizeBox (x, y, _) -> let
    box = Box2D {xMax2D = x, xMin2D = 0, yMax2D = y, yMin2D = 0}
    in genGrainDistributionByBox seed gsDist box

runVirMat2D :: JobRequest -> IO (Simulation Point2D)
runVirMat2D job = genPoints job >>= return . buildMicro

genPoints :: JobRequest -> IO (DistributedPoints Point2D)
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

buildMicro :: DistributedPoints Point2D -> Simulation Point2D
buildMicro (DistributedPoints box2D ps) = let
  ls     = V.length ps
  psID   = [0 .. ls - 1]
  wall   = fst $ runDelaunay2D box2D ps psID
  grains = mkVoronoiMicro (onlySimpleInBox2D box2D wall)
  in Simulation { box           = box2D
                , pointSet      = ps
                , triangulation = wall
                , grainSet      = grains }

onlySimpleInBox2D :: Box Point2D -> IM.IntMap (S2 Point2D) -> IM.IntMap (S2 Point2D)
onlySimpleInBox2D box = IM.filter (isInBox box . circleCenter)

--onlyDistInBox :: (PointND a)=> Box a -> SetPoint a -> SetPoint a
--onlyDistInBox box = V.filter (isInBox box . point)
