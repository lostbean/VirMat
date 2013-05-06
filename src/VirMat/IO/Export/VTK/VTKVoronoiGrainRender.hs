{-# LANGUAGE FlexibleInstances #-}

module VirMat.IO.Export.VTK.VTKVoronoiGrainRender
  ( writeVoronoiGrainVTKfile
  ) where

import qualified Data.IntMap         as IM
import qualified Data.Vector         as V
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as VU
  
import           Data.IntMap (IntMap, maxViewWithKey, findWithDefault)
import           Data.Vector (Vector, (!), cons, snoc, empty)

import           DeUni.DeWall
import           Hammer.Math.Algebra
import           Hammer.Render.VTK.VTKRender

import           VirMat.Core.VoronoiBuilder (VoronoiGrain(..), VoronoiFace(..))

type Simplex  = S2 Point3D
type VTKState = (Vector (Int, Int, Int, Int), Vector Vec3, Int, Vector Int)

writeVoronoiGrainVTKfile :: String -> IntMap Simplex -> [VoronoiGrain Point3D] -> IO ()
writeVoronoiGrainVTKfile file wall grains = let
  (cs, ps, _, ids) = renderGrains wall grains
  psU              = V.convert ps
  ug               = mkUGVTK "RegularTriangulation" psU cs
  ug'              = addDataCells ug $ mkCellAttr "GrainID" (\i _ _ -> ids!i)
  in writeUniVTKfile file ug'

renderGrains :: IntMap Simplex -> [VoronoiGrain Point3D] -> VTKState
renderGrains wall = L.foldl' renderGrain (empty, initPS, 0, empty)
  where
    len = case maxViewWithKey wall of
      Just (x,_) -> fst x
      _          -> 0
    ps     = IM.map circumSphereCenter wall  
    initPS = V.generate len (\i -> findWithDefault zero i ps)

renderGrain :: VTKState -> VoronoiGrain Point3D -> VTKState
renderGrain (tetras, sP, id, ids) grain = let
  len         = V.length sP
  centerGrain = grainCenter grain
  newTetras   = renderAllFaces len (faces grain)
  newIDs      = V.replicate (V.length newTetras) id
  in (tetras V.++ newTetras, sP `snoc` centerGrain, id + 1, ids V.++ newIDs)
  
renderAllFaces :: Int -> [VoronoiFace Point3D] -> Vector (Int, Int, Int, Int)
renderAllFaces offset faces = let
  tetras      = map (renderFace offset) faces
  in L.foldl' (V.++) empty tetras

renderFace :: Int -> VoronoiFace Point3D -> Vector (Int, Int, Int, Int)
renderFace centerGrain face = let
  ix  = map fst (edges face)
  
  func ref (x1:x2:xs) = (centerGrain, ref, x2, x1) `cons` func ref (x2:xs)
  func _ _ = V.empty
  
  in case ix of
    (ref:xs) -> func ref xs
    _          -> V.empty

instance RenderCell (Int, Int, Int, Int) where
  makeCell (a,b,c,d) = VU.fromList [a, b, c, d]
  getType _          = VTK_TETRA

