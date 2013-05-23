{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module VirMat.Core.FlexMicro
       ( FlexMicro (..)
       , mkFlexMicro
       ) where

import qualified Data.List           as L
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.IntSet         as IS
import qualified Data.Vector         as V

import           Data.Vector   (Vector)

import           Data.Maybe
import           Hammer.Math.Algebra
import           Hammer.MicroGraph
import           SubZero.SubTwo

import           VirMat.Core.VoronoiMicro

--import Debug.Trace
--dbg a = trace (">>" ++ show a) a

-- | Stores a flexible microstructure using Subdivision Surfaces. The topology and
-- the values are store separately in order to allow fast update of values. The topology is
-- represented by @MicroGraph@.  
data FlexMicro =
  FlexMicro
  { flexGraph  :: MicroGraph () MeshConn (Vector Int) Int -- ^ Microstructure graph
  , flexPoints :: Vector Vec3                             -- ^ Controls points (vertices, edges and faces)  
  } deriving (Show)

-- | This function converts a Voronoi microstructure (convex polygonal geomerty)
-- to flexible microstructure where arbitrary shape of grains are allowed.
-- Such a feature is provide by using Subdivision Surfaces.
mkFlexMicro :: VoronoiMicro Vec3 -> FlexMicro
mkFlexMicro vm = getFlexFace vm $ getFlexEdge vm $ getFlexVertex vm
               
getFlexVertex :: VoronoiMicro Vec3 -> FlexMicro
getFlexVertex MicroGraph{..} = let
  vs_clean = filter (hasPropValue . snd) $ HM.toList microVertex
  vh = HM.fromList $ zipWith (\(k, p) i -> (k, setPropValue p i)) vs_clean [0..]
  fp = V.fromList $ mapMaybe (getPropValue . snd) vs_clean
  fg = initMicroGraph { microVertex = vh, microGrains = microGrains }
  in FlexMicro fg fp

getFlexEdge ::  VoronoiMicro Vec3 -> FlexMicro -> FlexMicro
getFlexEdge vm@MicroGraph{..} (FlexMicro fm ps) = let
  psSize     = V.length ps
  es_clean    = mapMaybe foo $ HM.toList microEdges
  getter i m = getPropValue =<< getVertexProp i m
  foo (k, p) = do
    conn <- getPropConn p
    case conn of
      FullEdge a b -> do
        ia <- getter a fm
        ib <- getter b fm
        va <- getter a vm
        vb <- getter b vm
        return (k, (p, va, vb, ia, ib))
      _            -> Nothing
  fzip (k, (p, _, _, ia, ib)) i = (k, setPropValue p $ V.fromList [ia, i, ib])
  favg (_, (_, va, vb, _, _))   = 0.5 *& (va &+ vb)
  vv = V.fromList  $ map favg es_clean
  vh = HM.fromList $ zipWith fzip es_clean [psSize..]
  fg = fm { microEdges = vh }
  fp = ps V.++ vv
  in FlexMicro fg fp

getFlexFace :: VoronoiMicro Vec3 -> FlexMicro -> FlexMicro
getFlexFace MicroGraph{..} (FlexMicro fm ps) = let
  psSize   = V.length ps
  fs_clean = mapMaybe foo $ HM.toList microFaces
  getter i = getPropValue =<< getEdgeProp i fm
  foo (k, p) = do
    conn <- getPropConn p
    case mapMaybe getter (HS.toList conn) of
      [] -> Nothing
      es -> return (k, p, es)
  fzip (k, p, es) i = (k, setPropValue p $ mkMesh es i)
  vh = HM.fromList $ zipWith fzip fs_clean [psSize..]
  fp = V.fromList  $ map (\(_, _, es) -> faceCenter ps es) fs_clean
  fg = fm { microFaces = vh }
  in FlexMicro fg (ps V.++ fp)

mkMesh :: [Vector Int] -> Int -> MeshConn
mkMesh es fc = let
  getTris n vec
    | (n + 1) >= V.length vec = []
    | otherwise = (vec V.! n, vec V.! (n+1), fc) : getTris (n+1) vec
  ts = L.concatMap (getTris 0) es
  getCorners v
    | V.length v > 0 = IS.fromList [V.head v, V.last v]
    | otherwise      = IS.empty
  cornersSet = L.foldl' (\acc v -> acc `IS.union` getCorners v) IS.empty es
  corners = IS.toList cornersSet
  in buildMesh ts corners

-- | @faceCenter@ calculates the mass center of a given polygon defined by
-- a array of points and a list of edges (reference to array of points).
-- It expects a non-empty face otherwise a error will rise. DON'T EXPORT ME!!!
faceCenter :: Vector Vec3 -> [Vector Int] -> Vec3
faceCenter ps vs
  | n > 0     = total &* (1 / fromIntegral n)
  | otherwise = error "[FlexMicro] I can't calculate the center of a empty face!"
  where
    sumBoth (vacu, nacu) x = (vacu &+ sumFecth x, nacu + V.length x)
    sumFecth   = V.foldl' (\acc i -> acc &+ (ps V.! i)) zero
    (total, n) = L.foldl' sumBoth (zero, 0) vs
