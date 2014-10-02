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

-- ======================== Quadri Junction Force Simulation =============================
{--
  --writeFM "fm.vtu" fm 2
  let
    quads = IM.elems $ mapCP0D fm
    ps    = V.map controlPoint . V.fromList . IM.elems . controlPoints $ fm

  let
    upQ    = replicate 10 updateQuads
    quadFM = L.foldl' (\acc f -> f acc) fm upQ

  let
    ts = getAllGBTriangles fm 0
    ns = V.map getNormalTri ts
    plot = renderPoleFigureGB Lambert ns
    in renderSVGFile "pf.svg" (sizeSpec (Just 200, Nothing)) plot

  print "Get Stable Quadriple junctions..."
  writeFM "fmQuad.vtu" quadFM 2

  print "first angles"
  print $ listAngle fm

  print "first angles"
  print $ listAngle quadFM
--}

{--
getNormalTri :: (Vec3, Vec3, Vec3) -> Vec3
getNormalTri (a, b, c) = let
  ba = b &- a
  ca = c &- a
  in normalize $ ba &^ ca


updateQuads fm = let
  -- TODO solve the need for ps
  ps          = V.map controlPoint . V.fromList . IM.elems . controlPoints $ fm
  func acc i  = acc { controlPoints = IM.adjust (func2 i) i (controlPoints acc)}
  func2 i old = old { controlPoint = controlPoint old &+ (0.3 *& (calcForceAtQuad $ getPatchsAtQuadrs ps fm i))}
  quadriP     = mapCP0D fm
  in IM.foldl' func fm quadriP

getPatchsAtQuadrs ps fm id = case IM.lookup id (controlPoints fm) of
  Nothing -> []
  Just cp -> let
    sIDs   = S.elems $ surfaceMembers cp
    func x = do
      s            <- M.lookup x (surfaces fm)
      (patch, pos) <- findVertex (patchs s) id
      tn1          <- normalize <$> tan1 (evalPatch ps patch) pos
      tn2          <- normalize <$> tan2 (evalPatch ps patch) pos
      return (tn1 &+ tn2)
    in mapMaybe func sIDs

calcForceAtQuad ts
  | L.length ts >= 6 = L.foldl' (&+) zero ts
  | otherwise        = L.foldl' (&+) zero ts --zero


listAngle fm = concat . IM.elems . IM.map (getAngle fm) $ (mapCP0D fm)
getAngle fm id = let
    ps     = V.map controlPoint . V.fromList . IM.elems . controlPoints $ fm
    func x = do
      s            <- M.lookup x (surfaces fm)
      (patch, pos) <- findVertex (patchs s) id
      tn1          <- normalize <$> tan1 (evalPatch ps patch) pos
      tn2          <- normalize <$> tan2 (evalPatch ps patch) pos
      return (acos $ tn1 &. tn2)
    in case IM.lookup id (controlPoints fm) of
      Nothing -> []
      Just cp -> let
        sIDs   = S.elems $ surfaceMembers cp
        in mapMaybe func sIDs

 --}
