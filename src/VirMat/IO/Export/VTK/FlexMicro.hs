{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.IO.Export.VTK.FlexMicro
  ( writeFlexMicroVTK
  ) where

import qualified Data.Vector         as V
import qualified Data.HashSet        as HS
import qualified Data.Vector.Unboxed as VU
  
import           Data.Vector (Vector)

import           Hammer.Math.Algebra
import           Hammer.MicroGraph
import           Hammer.Render.VTK.VTKRender
import           SubZero.SubTwo

import           VirMat.Core.FlexMicro

writeFlexMicroVTK :: String -> Int -> FlexMicro -> IO ()
writeFlexMicroVTK file n grains = let
  vtk = renderGrains n grains
  in writeMultiVTKfile file vtk

renderGrains :: Int -> FlexMicro -> Vector (VTK Vec3)
renderGrains n fm@FlexMicro{..} = let
  gids    = V.fromList $ getGrainIDList flexGraph
  foo gid = renderGrain gid n fm
  in V.concatMap foo gids

renderGrain :: GrainID -> Int -> FlexMicro -> Vector (VTK Vec3)
renderGrain gid n fm@FlexMicro{..}  = let
  grainConn = getPropConn =<< getGrainProp gid flexGraph
  func acc fid = maybe acc (V.snoc acc) (renderFace fid gid n fm)
  foo = HS.foldl' func V.empty
  in maybe V.empty foo grainConn
     
renderFace :: FaceID -> GrainID -> Int -> FlexMicro -> Maybe (VTK Vec3)
renderFace fid gid n FlexMicro{..} = let
  func mesh = let
    sb   = mkSubTwo flexPoints mesh
    sbN  = subdivideTwoN n sb
    ps   = V.convert $ meshPoints sbN
    ts   = meshFaces $ meshConn sbN
    vtk  = mkUGVTK "FlexMicro" ps ts
    attr = mkCellAttr "GrainID" (\_ _ _ -> unGrainID gid)
    in return $ addDataCells vtk attr
  in func =<< getPropValue =<< getFaceProp fid flexGraph

instance RenderCell (Int, Int, Int, Int) where
  makeCell (a,b,c,d) = VU.fromList [a, b, c, d]
  getType _          = VTK_TETRA

