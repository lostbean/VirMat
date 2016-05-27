{-# LANGUAGE RecordWildCards #-}

module VirMat.PhaseTrans
  ( PhaseTransformation (..)
  , generateANG
  , generateTransformation
  ) where

import DeUni.Types
import File.ANGReader
import Hammer.MicroGraph
import Hammer.VTK (writeMultiVTKfile)
import Linear.Vect
import Texture.Bingham
import Texture.Orientation
import qualified Data.Vector as V
import qualified Data.List   as L
import qualified Data.IntMap as IM

import VirMat.Core.FlexMicro
import VirMat.Distributions.Texture.ODFSampling
import VirMat.IO.Export.ANG.RasterEngine
import VirMat.IO.Import.Types
import VirMat.Types
import VirMat.Run2D
import VirMat.Core.Sampling

data PhaseTransformation g
  = PhaseTransformation
  { microParent  :: FlexMicro Vec2 g
  , microProduct :: FlexMicro Vec2 g
  }

-- | Generate an ANG file with rasterization algorithm. Input: step size, simulation in 2D.
-- Output: square mode ANG.
generateANG :: Double -> Simulation Vec2 -> IO ANGdata
generateANG step Simulation{..} = let
  fm :: FlexMicro Vec2 ()
  fm = mkFlexMicro grainSet
  q1 = mkQuaternion $ Vec4 0 0 0 1
  q2 = mkQuaternion $ Vec4 0 0 1 0
  q3 = mkQuaternion $ Vec4 0 1 0 0
  dist = mkBingham (1, q1) (1, q2) (1, q3)
  in do
    fmTex <- addMicroFlexTexture dist fm
    return $ flexmicroToANG 0 step box fmTex

generateTransformation :: JobRequest -> IO (PhaseTransformation GrainID)
generateTransformation job = do
  simParent  <- runVirMat2D job
  simProduct <- runVirMat2D (job {gsDist = modDist 10})
  let
    microParent  = modifyGrainProps (\gid _ -> gid) $ mkFlexMicro (grainSet simParent)
    parentProps  = getParentProp 0 microParent simProduct
    microProduct = applyParentProp simProduct (mkGrainID (-1)) parentProps
  return PhaseTransformation
    { microParent  = microParent
    , microProduct = microProduct
    }

applyParentProp :: Simulation Vec2 -> g -> [(Int, g)] -> FlexMicro Vec2 g
applyParentProp simProduct nullprop props = let
  table      = IM.fromList props
  func gid _ = IM.findWithDefault nullprop (unGrainID gid) table
  in modifyGrainProps func $ mkFlexMicro (grainSet simProduct)

-- | Transform a microstructure to ANG using raster algorithm. INPUT: level of subdivision,
-- step size, bounding box, microstructure. OUTPUT: ANG data structure
getParentProp :: Int -> FlexMicro Vec2 g -> Simulation Vec2 -> [(Int, g)]
getParentProp n microParent simProduct = let
  gs = getGrainIDList (flexGraph2D microParent)
  cs = V.imap (\i p -> (i, point p)) (pointSet simProduct)
  filterInsideGrain acc gid = case getGrainMeshAndProp gid n microParent of
    Just (ps, ts, prop) -> V.toList xs ++ acc
      where
        xs = V.map (\(i,_) -> (i, prop)) (V.filter (func . snd) cs)
        func c = V.any (\(p1,p2,p3) -> isInsideTriangle (ps V.! p1, ps V.! p2, ps V.! p3) c) ts
    _ -> acc
  in L.foldl' filterInsideGrain [] gs

-- ================================================================================

testJob :: JobRequest
testJob = VoronoiJob
  { dimension    = Dimension2D
  , distrType    = RandomDistribution
  , gsDist       = [CombDist distJob]
  , seed         = Just 10
  , output       = Output "" "" []
  }

modDist :: Double -> [CombDist]
modDist k = [CombDist $ distJob {normalMean = 5 / k}]

distJob :: Normal
distJob = Normal
  { normalScale = 1
  , normalMean  = 5
  , normalVar   = 1
  }

runTest :: JobRequest -> IO ()
runTest job = do
  sim <- generateTransformation job
  let
    dir = "/Users/edgar/Desktop/"
    showTex = RenderGrainProp ("Value", \_ x -> fmap unGrainID x)
  writeMultiVTKfile (dir ++ "virmat-parent.vtu"  ) True . renderFlexMicro [showGrainID, showTex] 1 . microParent  $ sim
  writeMultiVTKfile (dir ++ "virmat-product2.vtu") True . renderFlexMicro [showGrainID, showTex] 1 . microProduct $ sim
