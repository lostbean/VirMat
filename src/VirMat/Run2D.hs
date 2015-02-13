{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Run2D where

import qualified Data.Vector as V
import qualified Data.IntMap as IM

import           DeUni.DeWall

import           VirMat.Types
import           VirMat.Core.VoronoiMicro
import           VirMat.Core.Packer
import           VirMat.Distributions.GrainSize.StatTools
import           VirMat.Distributions.GrainSize.GrainDistributionGenerator
import           VirMat.IO.Import.Types

--import           Debug.Trace
--debug  ::  Show a => String -> a -> a
--debug s x = trace (s ++ show x) x

-- ============================== 2D =====================================

runVirMat2D :: JobRequest -> IO (Simulation Point2D)
runVirMat2D jobReq = do
    (DistributedPoints box2D ps) <- case distrType jobReq of
        RandomDistribution       -> genFullRandomGrainDistribution (1, 1) jobReq
        PackedDistribution nstep -> do
          (DistributedPoints box2D ps0) <- genFullRandomGrainDistribution (1, 1) jobReq
          let
            len0         = V.length ps0
            psID0        = [0..len0-1]
            (wall0, _)   = runDelaunay2D box2D ps0 psID0
            (psFinal, _) = runPacker nstep box2D ps0 wall0
          return $ DistributedPoints box2D psFinal
    -- DEBUG
    let Just mdist = composeDist . gsDist $ jobReq
    print $ mDistMean mdist
    print $ mDistInterval mdist
    print $ mDistModes mdist
    print $ mDistArea mdist
    --print ps0
    -- =====
    let
      ls        = V.length ps
      psID      = [0 .. ls - 1]
      (wall, _) = runDelaunay2D box2D ps psID
      grains    = mkVoronoiMicro (onlySimpleInBox2D box2D wall)

    return $ Simulation { box           = box2D
                        , pointSet      = ps
                        , triangulation = wall
                        , grainSet      = grains }

onlyDistInBox :: (PointND a)=> Box a -> SetPoint a -> SetPoint a
onlyDistInBox box = V.filter (isInBox box . point)

onlySimpleInBox2D :: Box Point2D -> IM.IntMap (S2 Point2D) -> IM.IntMap (S2 Point2D)
onlySimpleInBox2D box = IM.filter (isInBox box . circleCenter)
