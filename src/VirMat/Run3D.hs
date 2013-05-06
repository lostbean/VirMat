{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Run3D where

import           VirMat.Types
import           VirMat.Core.VoronoiBuilder
import           VirMat.Core.Packer
import           VirMat.Distributions.GrainSize.StatTools
import           VirMat.Distributions.GrainSize.GrainDistributionGenerator
import           VirMat.IO.Import.Types

import qualified Data.Vector as V
import qualified Data.IntMap as IM

import           DeUni.DeWall

--import Debug.Trace
--debug :: Show a => String -> a -> a
--debug s x = trace (s ++ show x) x

-- ============================== 3D ======================================

runVirMat3D :: JobRequest -> IO (Simulation Point3D)
runVirMat3D jobReq = do
    (DistributedPoints box3D ps) <- case distrType jobReq of
        RandomDistribution -> genFullRandomGrainDistribution (1, 1, 1) jobReq
        PackedDistribution ->  do
          (DistributedPoints box3D ps0) <- genFullRandomGrainDistribution (1, 1, 1) jobReq
          let
            len0         = V.length ps0
            psID0        = [0..len0-1]
            (wall0, _)   = runDelaunay3D box3D ps0 psID0
            (psFinal, _) = runPacker 60 box3D ps0 wall0
          return $ DistributedPoints box3D psFinal
    -- === DEBUG ====
    let Just mdist = composeDist . gsDist $ jobReq
    print $ mDistMean mdist
    print $ mDistInterval mdist
    print $ mDistModes mdist
    print $ mDistArea mdist
    --print ps0
    -- =====
    let
      len       = V.length ps
      psID      = [0..len-1]
      (wall, _) = runDelaunay3D box3D ps psID
      grains    = convertDT2Voronoi ps (onlySimplexInBox3D box3D wall)

    return $ Simulation { box = box3D
                        , pointSet = ps
                        , triangulation = wall
                        , grainSet = grains }


onlyDistInBox :: (PointND a)=> Box a -> SetPoint a -> SetPoint a
onlyDistInBox box sp = V.filter ((isInBox box).point) sp

onlySimplexInBox3D :: Box Point3D -> IM.IntMap (S2 Point3D) -> IM.IntMap (S2 Point3D)
onlySimplexInBox3D box ls = IM.filter ((isInBox box).circumSphereCenter) ls

