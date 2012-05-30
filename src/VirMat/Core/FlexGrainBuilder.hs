{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module VirMat.Core.FlexGrainBuilder where

-- External modules
import qualified Data.List as L
import Data.List (null, foldl', sortBy)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap, mapWithKey)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.Vector as Vec
import Data.Vector (Vector, (!))
import Data.Maybe
import Control.Monad (liftM)
import Control.Monad.State

import Hammer.Math.Vector hiding (Vector)

import DeUni.Dim3.Base3D
import DeUni.Dim2.Base2D
import DeUni.Types

import SubZero.Mesh
import SubZero.Subdivide
import SubZero.Base hiding (Point3D)

import VirMat.Core.VoronoiBuilder


import Hammer.Render.VTK.VTKRender
import Hammer.Render.VTK.Base
import VirMat.Distributions.GrainSize.GrainDistributionGenerator


import Debug.Trace (trace)


type CP0DID = Int
type CP1DID = Int
type CP2DID = Int

type FlexFaceID = PairID
type FlexGrainID = Int

data FlexMicro = 
  FlexMicro
  { controlPoints :: IntMap ControlPoint
  , mapCP0D       :: IntMap Int
  , mapCP1D       :: Map PairID Int
  , mapCP2D       :: Map PairID Int
  , surfaces      :: Map FlexFaceID FlexFace
  , grains        :: IntMap FlexGrain
  } deriving (Show)

data ControlPoint =
  CP
  { controlPoint   :: Vec3
  , surfaceMembers :: Set FlexFaceID
  , grainMembers   :: IntSet
  } deriving (Show)

data FlexGrain =
  FlexGrain
  { grainID :: FlexGrainID
  , faces   :: [FlexFaceID]
  } deriving (Show)

data FlexFace =
  FlexFace
  { faceID             :: FlexFaceID
  , quadrupleJunction  :: [CP0DID]
  , tripleline         :: [CP1DID]
  , faceCP             :: CP2DID
  } deriving (Show)

type MicroState = State FlexMicro

emptyMicro = FlexMicro
  { controlPoints = IM.empty
  , mapCP0D       = IM.empty
  , mapCP1D       = M.empty
  , mapCP2D       = M.empty
  , surfaces      = M.empty
  , grains        = IM.empty
  }
    
newtype PairID = PairID (Int, Int) deriving (Show)

instance Ord PairID where
  compare = compPair

instance Eq PairID where
  x == y = compPair x y == EQ  
  
compPair (PairID (a1, a2)) (PairID (b1, b2))
  | amax > bmax = GT
  | amax < bmax = LT
  | amin > bmin = GT
  | amin < bmin = LT
  | otherwise   = EQ
  where
    amax = (eUp a1 a2)
    amin = (eBot a1 a2)
    bmax = (eUp b1 b2)
    bmin = (eBot b1 b2)
    eUp  x y = if compare x y == GT then x else y
    eBot x y = if compare x y == GT then y else x


makeMicroFlex::[VoronoiGrain Point3D] -> FlexMicro
makeMicroFlex gs = execState (mapM addGrain gs) emptyMicro
   
addGrain::VoronoiGrain Point3D -> MicroState ()
addGrain VoronoiGrain{..} = do
  faces <- mapM (addFace grainCenterIx) faces
  let ins = FlexGrain { grainID = grainCenterIx, faces = faces }
  modify (\x -> x { grains = IM.insert grainCenterIx  ins (grains x)})
  

combCP::ControlPoint -> ControlPoint -> ControlPoint
combCP cp1 cp2 = cp1 { surfaceMembers = surfaceMembers cp1 `S.union` surfaceMembers cp2
                     , grainMembers = grainMembers cp1 `IS.union` grainMembers cp2 }


addFace::Int -> VoronoiFace Point3D -> MicroState PairID
addFace grainID VoronoiFace{..} = let
  insCP id value = let
    ins = IM.insertWith combCP id (CP { controlPoint   = value
                                      , surfaceMembers = S.singleton (PairID (grainID, faceToIx))
                                      , grainMembers   = IS.singleton grainID
                                      })
    in do
      modify (\s -> s { controlPoints = ins $ controlPoints s})
      return id
      
  insCP0D edge = do
    micro@FlexMicro{..} <- get
    let
      ref   = fst edge
      value = circumOrigin $ snd edge
      newID = if IM.null controlPoints then 0 else 1 + (fst $ IM.findMax controlPoints)
    case IM.lookup ref mapCP0D of
      Just id -> insCP id value           
      Nothing -> do
        put micro{ mapCP0D = IM.insert ref newID mapCP0D }
        insCP newID value

  insCP1D interedge = do
    micro@FlexMicro{..} <- get
    let
      ref   = fst interedge
      value = snd interedge
      newID = if IM.null controlPoints then 0 else 1 + (fst $ IM.findMax controlPoints)
    case M.lookup ref mapCP1D of
      Just id -> insCP id value           
      Nothing -> do
        put micro{ mapCP1D = M.insert ref newID mapCP1D }
        insCP newID value
        
  insCP2D ref value = do
    micro@FlexMicro{..} <- get
    let
      newID = if IM.null controlPoints then 0 else 1 + (fst $ IM.findMax controlPoints)
    case M.lookup ref mapCP2D of
      Just id -> insCP id value           
      Nothing -> do
        put micro{ mapCP2D = M.insert ref newID mapCP2D }
        insCP newID value
        
         
  func (x1:x2:xs) = (PairID (fst x1, fst x2), 0.5 *& ((circumOrigin $ snd x1) &+ (circumOrigin $ snd x2))) : func (x2:xs)
  func [x1] = let
    x2 = head edges 
    in (PairID (fst x1, fst x2), 0.5 *& ((circumOrigin $ snd x1) &+ (circumOrigin $ snd x2))) : []
  
  in do
    cp0D <- mapM insCP0D edges
  
    cp1D <- mapM insCP1D (func edges)
  
    cp2D <- let
      (s, n) = foldl' (\(s, n) x -> (s &+ (circumOrigin $ snd x), n + 1)) (zero, 0) edges
      avg    = s &* (1/n)
      in insCP2D (PairID (grainID, faceToIx)) avg
    
    micro@FlexMicro{..} <- get
    case M.lookup (PairID (grainID, faceToIx)) surfaces of
      Just face -> return () 
      Nothing   -> put $ micro { surfaces = M.insert (PairID (grainID, faceToIx)) ( FlexFace { faceID = (PairID (grainID, faceToIx))
                                                                                             , quadrupleJunction  = cp0D
                                                                                             , tripleline         = cp1D
                                                                                             , faceCP             = cp2D
                                                                                             } ) surfaces }
    return (PairID (grainID, faceToIx))
                   


writeFM out fm n func = let
  vtks = renderFlexMicro' fm n func
  in writeMultiVTKfile (text2Path out) vtks


renderFlexMicro::FlexMicro -> Int -> (Vector Point3D -> Vector Point3D) -> Vector (VTK Point3D)
renderFlexMicro FlexMicro{..} n trans = let
  ps = Vec.fromList $ IM.elems controlPoints
  ps' = trans $ Vec.map controlPoint ps
  faces = M.elems surfaces
  patchs = concatMap (makePatch ps') faces

  patchs' = foldl (\acc x -> x acc) patchs (take n $ repeat subdAll)
  subdAll = map subdivide
  in Vec.fromList $ map renderPatch patchs'


renderFlexMicro'::FlexMicro -> Int -> (Vector Point3D -> Vector Point3D) -> Vector (VTK Point3D)
renderFlexMicro' FlexMicro{..} n trans = let
  ps = Vec.fromList $ IM.elems controlPoints
  ps' = trans $ Vec.map controlPoint ps
  gs = Vec.fromList $ IM.elems grains
  in Vec.concatMap (\FlexGrain{..} -> let
          fs = mapMaybe (\k -> M.lookup k surfaces) faces
          ids x  = addDataCells x (IDDataCell "GrainID" (\ n v c -> grainID))
          patchs = concatMap (makePatch ps') fs

          patchs' = foldl (\acc x -> x acc) patchs (take n $ repeat subdAll)
          subdAll = map subdivide
          in Vec.map ids $ Vec.fromList $ map renderPatch patchs') gs


makePatch::Vector Point3D -> FlexFace -> [Patch Point3D]
makePatch ps FlexFace{..} = let
  edges  = getEdges quadrupleJunction tripleline
  tris   = map (\(a,b) -> (a,b,faceCP)) edges
  mesh   = buildMesh tris
  in map (evalPatch ps . makepatch mesh quadrupleJunction) tris 

getEdges a b = let
  ahead = L.head a

  intercalate []     _      = []
  intercalate _     []      = []
  intercalate (x:x1:xs) (y:ys) = (x, y):(y, x1):(intercalate (x1:xs) (ys))
  intercalate (x:xs) (y:ys) = (x, y):(y, ahead):(intercalate (xs) (ys))
  
  in intercalate a b
     