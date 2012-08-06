{-# LANGUAGE FlexibleInstances #-}

module VirMat.IO.Export.VTK.VTKVoronoiGrainRender
       ( writeVoronoiGrainVTKfile
       ) where

import Prelude hiding ((++))
import Data.IntMap (IntMap, maxViewWithKey, findWithDefault)
import qualified Data.IntMap as IM
import Data.Vector (Vector, (!), cons, snoc, (++), empty, foldl')
import qualified Data.Vector as Vec

import Hammer.Math.Vector hiding (Vector)
import Hammer.Render.VTK.VTKRender
import Hammer.Render.VTK.Base

import DeUni.DeWall

import VirMat.Core.VoronoiBuilder (VoronoiGrain(..), VoronoiFace(..))

type Simplex = S2 Point3D
type VTKState = (Vector (Int, Int, Int, Int), Vector Vec3, Int, Vector Int)

writeVoronoiGrainVTKfile::String -> IntMap Simplex -> [VoronoiGrain Point3D] -> IO ()
writeVoronoiGrainVTKfile file wall grains = let
  (cs, ps, _, ids) = renderGrains wall grains
  ug               = mkUGVTK "RegularTriangulation" ps cs
  ug'              = addDataCells ug $ IDDataCell "GrainID" (\i _ _ -> ids!i)
  in writeUniVTKfile (text2Path file) ug'

renderGrains::IntMap Simplex -> [VoronoiGrain Point3D] -> VTKState
renderGrains wall grains = let
  len = case maxViewWithKey wall of
    Just (x,_) -> fst x
    _          -> 0
  ps = IM.map circumSphereCenter wall  
  initPS = Vec.generate len (\i -> findWithDefault zero i ps)
  in foldl renderGrain (empty, initPS, 0, empty) grains

renderGrain::VTKState -> VoronoiGrain Point3D -> VTKState
renderGrain (tetras, sP, id, ids) grain = let
  len         = Vec.length sP
  centerGrain = grainCenter grain
  newTetras   = renderAllFaces len (faces grain)
  newIDs      = Vec.replicate (Vec.length newTetras) id
  in (tetras ++ newTetras, sP `snoc` centerGrain, id+1, ids ++ newIDs)
  
renderAllFaces::Int -> [VoronoiFace Point3D] -> Vector (Int, Int, Int, Int)
renderAllFaces offset faces = let
  tetras      = map (renderFace offset) faces
  in foldl (++) empty tetras

renderFace::Int -> VoronoiFace Point3D -> Vector (Int, Int, Int, Int)
renderFace centerGrain face = let
  ix  = map fst (edges face)
  
  func ref (x1:x2:xs) = (centerGrain, ref, x2, x1) `cons` func ref (x2:xs)
  func _ _ = Vec.empty
  
  in case ix of
    (ref:xs) -> func ref xs
    _          -> Vec.empty

   
instance RenderCell (Int, Int, Int, Int) where
  makeCell (a,b,c,d) = a `cons` (b `cons` (c `cons` Vec.singleton d))
  getType _ = VTK_TETRA

