{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module VirMat.IO.Export.VTK.FlexMicro
       ( RenderGrainProp (..)
       , renderFlexMicro
       , showGrainID
       ) where

import qualified Data.HashSet        as HS
import qualified Data.List           as L
import qualified Data.Vector         as V

import           Control.Applicative ((<$>))
import           Data.Vector         (Vector)

import           Hammer.Math.Algebra
import           Hammer.MicroGraph
import           Hammer.Render.VTK.VTKRender
import           SubZero

import           VirMat.Core.FlexMicro

-- | Creates an attribute generator for each grain in a 'FlexMicro' structure. It needs a
-- name for the generated attribute, that be will identified on VTK visualization software,
-- and a function that generates the attribute itself based on 'GrainID' and 'FlexMicro'.
data RenderGrainProp g = forall a . RenderElemVTK a =>
                         RenderGrainProp (String, GrainID -> FlexMicro g -> Maybe a)

addGrainAttrs :: (RenderElemVTK a)=> GrainID -> FlexMicro g
              -> [RenderGrainProp g] -> VTK a -> VTK a
addGrainAttrs gid fm renders vtk = let
  foo (RenderGrainProp (name, func)) = let
    add x v = addDataCells v (mkCellAttr name (\_ _ _ -> x))
    in maybe id add (func gid fm)
  in L.foldl' (\acc x -> foo x acc) vtk renders

-- | Render 'FlexMicro' to 'VTK'. The extra info (attributes) are produced by a list of
-- attribute generators 'RenderGrainProp'. The surfaces are rendered with @n@ levels of
-- subdivision.
renderFlexMicro :: [RenderGrainProp g] -> Int -> FlexMicro g -> Vector (VTK Vec3)
renderFlexMicro renders n fm@FlexMicro{..} = let
  gids    = V.fromList $ getGrainIDList flexGraph
  foo gid = renderGrain gid renders n fm
  in V.concatMap foo gids

renderGrain :: GrainID -> [RenderGrainProp g] -> Int -> FlexMicro g -> Vector (VTK Vec3)
renderGrain gid renders n fm@FlexMicro{..}  = let
  grainConn  = getPropConn =<< getGrainProp gid flexGraph
  getVTK fid = addGrainAttrs gid fm renders <$> renderFace fid n fm
  func acc   = maybe acc (V.snoc acc) . getVTK
  in maybe V.empty (HS.foldl' func V.empty) grainConn

renderFace :: FaceID -> Int -> FlexMicro a -> Maybe (VTK Vec3)
renderFace fid n FlexMicro{..} = let
  func mesh = let
    sb  = mkSubTwoFromMesh flexPoints mesh
    sbN = subdivideN n sb
    ps  = V.convert $ subTwoPoints sbN
    ts  = getSubTwoFaces $ subTwoMesh sbN
    in mkUGVTK "FlexMicro" ps ts
  in func <$> (getPropValue =<< getFaceProp fid flexGraph)

-- | Show the 'GrainID' value in the 'VTK' data.
showGrainID :: RenderGrainProp g
showGrainID = RenderGrainProp ("GrainID", \gid _ -> return $ unGrainID gid)
