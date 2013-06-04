{-# LANGUAGE  RecordWildCards #-}

module Main where

import qualified Data.List   as L
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S
import qualified Data.Vector as V

import           System.Environment (getArgs)
  
import           Control.Applicative
import           Data.Maybe
import           Hammer.Math.Algebra
import           Hammer.Math.SphereProjection
import           SubZero.SubTwo

import           VirMat.IO.Import.CommandLineInput
import           VirMat.Core.FlexMicro
import           VirMat.Run2D
import           VirMat.Run3D
import           VirMat.Types
import           VirMat.IO.Import.Types
import           VirMat.Distributions.GrainSize.GrainDistributionGenerator
import           VirMat.IO.Export.SVG.RenderSVG
import           VirMat.IO.Export.VTK.FlexMicro

import           Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

main :: IO ()
main = do
  putStrLn ("VirMat v0.3 04/2012 [by Edgar Gomes]")
  putStrLn  "____________________________________"
  jobReq <- getArgs >>= (return.parseArgs)
  case dimension jobReq of
    Dimension2D -> go2D jobReq
    Dimension3D -> go3D jobReq

-- ============================== 3D ====================================== 

go3D :: JobRequest -> IO ()
go3D jobReq = do
  simul <- runVirMat3D jobReq
  let fm = mkFlexMicro $ grainSet simul
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
--}

  rand <- randomSO3 jobReq 3000
  renderSVGFile "pfRandStero.svg" (sizeSpec (Just 200, Nothing)) $ renderPoleFigureGB Sterographic rand
  renderSVGFile "pfRandLambe.svg" (sizeSpec (Just 200, Nothing)) $ renderPoleFigureGB Lambert rand

  print "Get Points..."
  --writeWPointsVTKfile "points.vtu" (pointSet simul)
  
  print "Get Voronoi..."
  writeFlexMicroVTK "voronoi.vtu"   0 fm
  writeFlexMicroVTK "voronoi_1.vtu" 1 fm

  print "Get Stable Quadriple junctions..."
  -- writeFM "fmQuad.vtu" quadFM 2

  print "first angles"
  -- print $ listAngle fm

  print "first angles"
  -- print $ listAngle quadFM

-- =========================== 2D ========================================== 

go2D :: JobRequest -> IO ()
go2D jobReq = do
  simul <- runVirMat2D jobReq
  print "Fix SVG render."
  --printMicro "Final" simul



{--
-- ============================== Quadri Junction Force Simulation =====================================

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