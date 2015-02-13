{-# LANGUAGE  RecordWildCards #-}

module Main where

import           Hammer.VTK (writeMultiVTKfile)

import           Hammer.Math.Algebra
import           Texture.Bingham
import           Texture.Orientation
import           Texture.Symmetry

import           VirMat.Core.FlexMicro
import           VirMat.Distributions.Texture.ODFSampling
import           VirMat.Distributions.GrainSize.GrainQuery
import           VirMat.IO.Import.CommandLine
import           VirMat.IO.Import.Types
import           VirMat.Run2D
import           VirMat.Run3D
import           VirMat.Types

import           Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

main :: IO ()
main = do
  jobReq <- getJob
  case dimension jobReq of
    Dimension2D -> go2D jobReq
    Dimension3D -> go3D jobReq

-- ====================================== 3D =============================================

go3D :: JobRequest -> IO ()
go3D jobReq = do
  simul <- runVirMat3D jobReq
  let
    fm :: FlexMicro Vec3 ()
    fm = mkFlexMicro $ grainSet simul
    q1 = mkQuaternion $ Vec4 0 0 0 1
    q2 = mkQuaternion $ Vec4 0 0 1 0
    q3 = mkQuaternion $ Vec4 0 1 0 0
    dist = mkBingham (1, q1) (1, q2) (1, q3)
  fmTex <- addMicroFlexTexture dist fm
  let
    fmMorphTex = add3DGrainMorph 2 fmTex
    showVolu = RenderGrainProp ("Volume", \_ x -> fmap (getVolume . grainVolume . fst) x)
    showArea = RenderGrainProp ("Area", \_ x -> fmap (getArea . grainArea . fst) x)
    showNeig = RenderGrainProp ("Neighbors", \_ x -> fmap (grainNeighbors . fst) x)
    showTex  = RenderGrainProp ("IPF-ND", \_ x -> fmap (getIPFRGBColor Cubic ND . snd) x)
    showall = [showGrainID, showVolu, showArea, showNeig, showTex]

  writeMultiVTKfile "virmat-3d.vtu" True $ renderFlexMicro showall 1 fmMorphTex

-- ========================================== 2D =========================================

go2D :: JobRequest -> IO ()
go2D jobReq = do
  simul <- runVirMat2D jobReq
  let
    fm :: FlexMicro Vec2 ()
    fm = mkFlexMicro $ grainSet simul
    fmMorph = add2DGrainMorph 2 fm
    showLeng = RenderGrainProp ("Length", \_ x -> fmap (getLength . grainLength . fst) x)
    showArea = RenderGrainProp ("Area", \_ x -> fmap (getArea . grainArea . fst) x)
    showNeig = RenderGrainProp ("Neighbors", \_ x -> fmap (grainNeighbors . fst) x)
    showall = [showGrainID, showLeng, showArea, showNeig]
  writeMultiVTKfile "virmat-2d.vtu" True $ renderFlexMicro showall 1 fmMorph
