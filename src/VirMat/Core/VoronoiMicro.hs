{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module VirMat.Core.VoronoiMicro
  ( mkVoronoiMicro
  , VoronoiMicro
  ) where

import qualified Data.IntMap as IM

import           Data.IntMap   (IntMap)

import           Hammer.Math.Algebra
import           Hammer.MicroGraph

import           DeUni.Dim3.Base3D (tetraPoints)
import           DeUni.Dim2.Base2D (face2DPoints)
import           DeUni.Types

--import Debug.Trace (trace)

class VoronoiMicroBuilder v where
  type VoronoiMicro v
  mkVoronoiMicro :: IntMap (S2 v) -> VoronoiMicro v

-- =======================================================================================

instance VoronoiMicroBuilder Vec3 where
  type VoronoiMicro Vec3 = MicroGraph () () () Vec3
  --mkVoronoiMicro :: IntMap (S2 Vec3) -> VoronoiMicro3D
  mkVoronoiMicro = IM.foldl' (flip addS2) initMicroGraph

addS2 :: S2 Vec3 -> VoronoiMicro Vec3 -> VoronoiMicro Vec3
addS2 s2 = let
  tetra@(a,b,c,d) = tetraPoints s2
  vid = mkVertexID tetra
  v   = circumOrigin s2
  fab = mkFaceID (a, b)
  fbc = mkFaceID (b, c)
  fca = mkFaceID (c, a)
  fad = mkFaceID (a, d)
  fbd = mkFaceID (b, d)
  fcd = mkFaceID (c, d)

  ea = mkEdgeID' (fbc, fbd, fcd)
  eb = mkEdgeID' (fca, fad, fcd)
  ec = mkEdgeID' (fab, fad, fbd)
  ed = mkEdgeID' (fab, fbc, fca)

  addV = insertNewVertex vid v [ea, eb, ec, ed]

  addEa = insertEdgeConn ea [fbc, fbd, fcd]
  addEb = insertEdgeConn eb [fca, fad, fcd]
  addEc = insertEdgeConn ec [fab, fad, fbd]
  addEd = insertEdgeConn ed [fab, fbc, fca]

  addFab = insertFaceConn fab
  addFbc = insertFaceConn fbc
  addFca = insertFaceConn fca
  addFad = insertFaceConn fad
  addFbd = insertFaceConn fbd
  addFcd = insertFaceConn fcd

  in addV . addEa . addEb . addEc . addEd .
     addFab . addFbc . addFca . addFad . addFbd . addFcd

-- =======================================================================================

instance VoronoiMicroBuilder Vec2 where
  type VoronoiMicro Vec2 = MicroGraph () () Vec2 ()
  --mkVoronoiMicro2D :: IntMap (S2 Vec2) -> VoronoiMicro2D
  mkVoronoiMicro = IM.foldl' (flip addS22D) initMicroGraph

addS22D :: S2 Vec2 -> VoronoiMicro Vec2 -> VoronoiMicro Vec2
addS22D s2 = let
  (a, b, c) = face2DPoints s2

  v   = circumOrigin s2

  fab = mkFaceID (a, b)
  fbc = mkFaceID (b, c)
  fca = mkFaceID (c, a)

  e = mkEdgeID' (fab, fbc, fca)

  addE = insertNewEdge e v [fab, fbc, fca]

  addFab = insertFaceConn fab
  addFbc = insertFaceConn fbc
  addFca = insertFaceConn fca

  in addE . addFab . addFbc . addFca
