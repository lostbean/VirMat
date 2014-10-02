{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VirMat.Distributions.GrainSize.GrainQuery
  ( Volume (getVolume)
  , Area   (getArea)
  , Length (getLength)
  , GrainMorph ( grainCenter
               , grainLength
               , grainArea
               , grainVolume
               , grainNeighbors
               )
  , add3DGrainMorph
  , add2DGrainMorph
  ) where

import qualified Data.Vector  as V
import qualified Data.HashSet as HS

import           Data.Vector   (Vector)
import           Data.Maybe    (mapMaybe)
import           Data.List     (foldl')

import           Hammer.Math.Algebra
import           Hammer.MicroGraph
import           SubZero

import           VirMat.Core.FlexMicro

newtype Length = Length
  { getLength :: Double
  } deriving (Eq, Ord, Num, Fractional, Show)

newtype Area = Area
  { getArea :: Double
  } deriving (Eq, Ord, Num, Fractional, Show)

newtype Volume = Volume
  { getVolume :: Double
  } deriving (Eq, Ord, Num, Fractional, Show)

data GrainMorph v
  = GrainMorph
  { grainCenter    :: v
  , grainLength    :: Length
  , grainArea      :: Area
  , grainVolume    :: Volume
  , grainNeighbors :: Int
  } deriving (Show)

nullmorph :: (AbelianGroup v)=> GrainMorph v
nullmorph = GrainMorph
  { grainCenter    = zero
  , grainLength    = 0
  , grainArea      = 0
  , grainVolume    = 0
  , grainNeighbors = 0
  }

add2DGrainMorph :: Int -> FlexMicro Vec2 a -> FlexMicro Vec2 (GrainMorph Vec2, a)
add2DGrainMorph n fm@FlexMicro2D{..} = modifyGrainProps func fm
  where
    func _ prop = let
      m = maybe nullmorph genmorph (getPropConn prop)
      x = maybe undefined id       (getPropValue prop)
      in (m, x)
    genmorph fs = let
      foo fid = getFaceProp fid flexGraph2D >>= getPropValue
      ms = V.fromList $ mapMaybe foo (HS.toList fs)
      center = calc2DGrainCenter        flexPoints2D ms n
      glen   = calc2DGrainLength        flexPoints2D ms n
      area   = calc2DGrainArea   center flexPoints2D ms n
      in GrainMorph
         { grainCenter    = center
         , grainLength    = glen
         , grainArea      = area
         , grainVolume    = 0
         , grainNeighbors = HS.size fs
         }

add3DGrainMorph :: Int -> FlexMicro Vec3 a -> FlexMicro Vec3 (GrainMorph Vec3, a)
add3DGrainMorph n fm@FlexMicro3D{..} = modifyGrainProps func fm
  where
    func _ prop = let
      m = maybe nullmorph genmorph (getPropConn prop)
      x = maybe undefined id       (getPropValue prop)
      in (m, x)
    genmorph fs = let
      foo fid = getFaceProp fid flexGraph3D >>= getPropValue
      ms = V.fromList $ mapMaybe foo (HS.toList fs)
      center = calc3DGrainCenter        flexPoints3D ms n
      area   = calc3DGrainArea          flexPoints3D ms n
      vol    = calc3DGrainVolume center flexPoints3D ms n
      in GrainMorph
         { grainCenter    = center
         , grainLength    = 0
         , grainArea      = area
         , grainVolume    = vol
         , grainNeighbors = HS.size fs
         }

-- =======================================================================================

calc2DGrainLength :: (AbelianGroup v, DotProd v, SubZero (SubOne v))=>
                     Vector v -> Vector (Vector Int) -> Int -> Length
calc2DGrainLength vs vmesh n = V.foldl' (\acc m -> acc + calcSubOneLength vs m n) 0 vmesh

calc2DGrainArea :: Vec2 -> Vector Vec2 -> Vector (Vector Int) -> Int -> Area
calc2DGrainArea = calcSubOneArea

calc2DGrainCenter :: Vector Vec2 -> Vector (Vector Int) -> Int -> Vec2
calc2DGrainCenter vs vmesh n
  | V.null vmesh = zero
  | otherwise    = (1/l) *& V.foldl' func zero vmesh
  where
    l = fromIntegral $ V.length vmesh
    func acc m = acc &+ calcCentroidSubOne vs m n

-- =======================================================================================

calc3DGrainArea :: Vector Vec3 -> Vector MeshConn -> Int -> Area
calc3DGrainArea vs vmesh n = V.foldl' (\acc m -> acc + calcSubTwoArea vs m n) 0 vmesh

calc3DGrainVolume :: Vec3 -> Vector Vec3 -> Vector MeshConn -> Int -> Volume
calc3DGrainVolume d vs vmesh n = V.foldl' func 0 vmesh
  where func acc m = acc + calcSubTwoVolume d vs m n

calc3DGrainCenter :: Vector Vec3 -> Vector MeshConn -> Int -> Vec3
calc3DGrainCenter vs vmesh n
  | V.null vmesh = zero
  | otherwise    = (1/l) *& V.foldl' func zero vmesh
  where
    l = fromIntegral $ V.length vmesh
    func acc m = acc &+ calcCentroidSubTwo vs m n

-- =======================================================================================

calcCentroidSubOne :: (SubZero (SubOne v), MultiVec v)=>
                      Vector v -> Vector Int -> Int -> v
calcCentroidSubOne vs vi n = maybe zero func (mkSubOne vi vs)
  where
    func sb = let
      sbN = subdivideN n sb
      ps  = subOnePoints sbN
      in getCentroid ps

calcCentroidSubTwo :: Vector Vec3 -> MeshConn -> Int -> Vec3
calcCentroidSubTwo vs mesh n = let
  sb  = mkSubTwoFromMesh vs mesh
  sbN = subdivideN n sb
  ps  = subTwoPoints sbN
  in getCentroid ps

calcSubTwoArea :: Vector Vec3 -> MeshConn -> Int -> Area
calcSubTwoArea = withSubTwoTriangles (\acc a b c -> acc + triangleArea a b c) 0

calcSubTwoVolume :: Vec3 -> Vector Vec3 -> MeshConn -> Int -> Volume
calcSubTwoVolume d = withSubTwoTriangles (\acc a b c -> acc + tetrahedronVolume a b c d) 0

calcSubOneLength :: (AbelianGroup v, DotProd v, SubZero (SubOne v))=>
                    Vector v -> Vector Int -> Int -> Length
calcSubOneLength vs vi n = Length $ maybe 0 func (mkSubOne vi vs)
  where
    func sb = let
      sbN = subdivideN n sb
      ps  = subOnePoints sbN
      foo (acc, x0) x1 = (acc + norm (x1 &- x0), x1)
      -- ps > 2 guaranteed by 'mkSubOne'
      in fst $ V.foldl' foo (0, V.head ps) (V.tail ps)

calcSubOneArea :: Vec2 -> Vector Vec2 -> Vector (Vector Int) -> Int -> Area
calcSubOneArea center vs vvi n = V.foldl' (\acc vi -> acc + foo vi) 0 vvi
  where
    foo vi = maybe 0 func (mkSubOne vi vs)
    func sb = let
      sbN = subdivideN n sb
      ps  = subOnePoints sbN
      f (acc, x0) x1 = (acc + triangleArea x1 x0 center, x1)
      -- ps > 2 guaranteed by 'mkSubOne'
      in fst $ V.foldl' f (0, V.head ps) (V.tail ps)

withSubTwoTriangles :: (a -> Vec3 -> Vec3 -> Vec3 -> a) -> a -> Vector Vec3 -> MeshConn -> Int -> a
withSubTwoTriangles func x0 vs mesh n = let
  sb  = mkSubTwoFromMesh vs mesh
  sbN = subdivideN n sb
  ps  = subTwoPoints sbN
  ts  = getSubTwoFaces $ subTwoMesh sbN
  in V.foldl' (\acc (a, b, c) -> func acc (ps V.! a) (ps V.! b) (ps V.! c)) x0 ts

-- =======================================================================================

triangleArea :: (AbelianGroup a, DotProd a)=> a -> a -> a -> Area
triangleArea a b c = let
  ca = a &- c
  cb = b &- c
  dot = ca &. cb
  x   = normsqr cb * normsqr ca - dot * dot
  in Area (0.5 * sqrt x)

tetrahedronVolume :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Volume
tetrahedronVolume a b c d = Volume $ abs $ ((a &- d) &. ((b &- d) &^ (c &- d))) / 6

getCentroid :: (AbelianGroup a, MultiVec a)=> Vector a -> a
getCentroid v = (V.foldl' (&+) zero v) &* k
  where k = 1 / (fromIntegral $ V.length v)

{--  Unused but useful functions 
triangleNormal :: (AbelianGroup a, DotProd a, CrossProd a, MultiVec a)=> a -> a -> a -> a
triangleNormal a b c = let
  ca = a &- c
  cb = b &- c
  in normalize $ ca &^ cb

signedTriangleVolume :: (AbelianGroup a, DotProd a, CrossProd a)=> a -> a -> a -> Area
signedTriangleVolume a b c = Area $ (a &. (b &^ c)) / 6

getAverage :: (Num a, Fractional a)=> [a] -> a
getAverage gs = let
  (total, n) = foldl' (\(vacc, nacc) x -> (vacc + x, nacc + 1)) (0, 0) gs
  in total / n
--}
