{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module VirMat.Core.FlexMicro
       ( FlexMicro (..)
       , mkFlexMicro
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.IntSet         as IS
import qualified Data.Vector         as V

import           Data.Vector         (Vector)

import           Data.Maybe
import           Hammer.Math.Algebra
import           Hammer.MicroGraph
import           Hammer.Math.SortSeq
import           SubZero

import           VirMat.Core.VoronoiMicro

--import Debug.Trace
--dbg a = trace (">>" ++ show a) a

-- | Stores a flexible microstructure using Subdivision Surfaces. The topology and
-- the values are store separately in order to allow fast update of values. The topology is
-- represented by @MicroGraph@.
data FlexMicro a =
  FlexMicro
  { flexGraph  :: MicroGraph a MeshConn (Vector Int) Int -- ^ Microstructure graph
  , flexPoints :: Vector Vec3                            -- ^ Controls points (vertices, edges and faces)
  } deriving (Show)

-- | This function converts a Voronoi microstructure (convex polygonal geomerty)
-- to flexible microstructure where arbitrary shape of grains are allowed.
-- Such a feature is provide by using Subdivision Surfaces.
mkFlexMicro :: VoronoiMicro Vec3 -> FlexMicro ()
mkFlexMicro vm = getFlexFace vm $ getFlexEdge vm $ getFlexVertex vm

getFlexVertex :: VoronoiMicro Vec3 -> FlexMicro ()
getFlexVertex MicroGraph{..} = let
  vs_clean = filter (hasPropValue . snd) $ HM.toList microVertex
  vh = HM.fromList $ zipWith (\(k, p) i -> (k, setPropValue p i)) vs_clean [0..]
  fp = V.fromList $ mapMaybe (getPropValue . snd) vs_clean
  fg = initMicroGraph { microVertex = vh, microGrains = microGrains }
  in FlexMicro fg fp

getFlexEdge ::  VoronoiMicro Vec3 -> FlexMicro a -> FlexMicro a
getFlexEdge vm@MicroGraph{..} (FlexMicro fm ps) = let
  psSize     = V.length ps
  es_clean   = fst $ HM.foldlWithKey' foo (V.empty, psSize) microEdges
  getter i m = getPropValue =<< getVertexProp i m
  foo acc@(vec, n) k p = maybe acc id $ do
    conn <- getPropConn p
    case conn of
      FullEdge a b -> do
        ia <- getter a fm
        ib <- getter b fm
        va <- getter a vm
        vb <- getter b vm
        let prop = setPropValue p $ V.fromList [ia, n, ib]
            avg  = 0.5 *& (va &+ vb)
            v    = (k, avg, prop)
        return (vec `V.snoc` v, n + 1)
      _            -> Nothing
  vv = V.map (\(_,x,_) -> x) es_clean
  vh = HM.fromList . V.toList $ V.map (\(k,_,p) -> (k,p)) es_clean
  fg = fm { microEdges = vh }
  fp = ps V.++ vv
  in FlexMicro fg fp

getFlexFace :: VoronoiMicro Vec3 -> FlexMicro a -> FlexMicro a
getFlexFace MicroGraph{..} (FlexMicro fm ps) = let
  psSize   = V.length ps
  fs_clean = fst $ HM.foldlWithKey' foo (V.empty, psSize) microFaces
  getEdge eid = getPropValue =<< getEdgeProp eid fm
  foo acc@(vec, n) k p = maybe acc id $ do
    conn <- getPropConn p
    let es = HS.foldl' (\acu eid -> maybe acu (V.snoc acu) (getEdge eid)) V.empty conn
    mesh <- mkMesh es n
    fc   <- faceCenter ps es
    let v = (k, setPropValue p mesh, fc)
    return (vec `V.snoc` v, n + 1)
  vh = HM.fromList . V.toList $ V.map (\(k,m,_) -> (k,m)) fs_clean
  fp = V.map (\(_,_,p) -> p) fs_clean
  fg = fm { microFaces = vh }
  in FlexMicro fg (ps V.++ fp)

mkMesh :: Vector (Vector Int) -> Int -> Maybe MeshConn
mkMesh es fc = do
  ses <- sortEdges es
  return $ buildMesh (toTS ses) corners
  where
    getTris vec = V.imap (\i x -> (x, vec V.! (i+1), fc)) (V.init vec)
    toTS        = V.toList . V.concatMap getTris
    getCorners v
      | V.length v > 0 = IS.fromList [V.head v, V.last v]
      | otherwise      = IS.empty
    cornersSet = V.foldl' (\acc v -> acc `IS.union` getCorners v) IS.empty es
    corners    = IS.toList cornersSet

-- | @faceCenter@ calculates the mass center of a given polygon defined by
-- a array of points and a list of edges (reference to array of points).
-- It expects a non-empty face otherwise a error will rise. DON'T EXPORT ME!!!
faceCenter :: Vector Vec3 -> Vector (Vector Int) -> Maybe Vec3
faceCenter ps vs
  | n > 0     = return $ total &* (1 / fromIntegral n)
  | otherwise = Nothing
  where
    sumBoth (vacu, nacu) x = (vacu &+ sumFecth x, nacu + V.length x)
    sumFecth   = V.foldl' (\acc i -> acc &+ (ps V.! i)) zero
    (total, n) = V.foldl' sumBoth (zero, 0) vs

sortEdges :: (SeqSeg a)=> Vector a -> Maybe (Vector a)
sortEdges = getOneLoop . sortSegs

instance SeqComp Int

instance (SeqComp a)=> SeqSeg (Vector a) where
  type SeqUnit (Vector a) = a
  getSeqHead = V.head
  getSeqTail = V.last
  seqInv     = V.reverse
