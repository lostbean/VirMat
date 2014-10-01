{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module VirMat.Core.FlexMicro
       ( FlexMicroBuilder
       , FlexMicro (..)
       , mkFlexMicro
       , renderFlexMicro
       , modifyGrainProps
       , flexGrainList
       -- * Rendering
       , RenderGrainProp (..)
       , addGrainAttrs
       , showGrainID
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.IntSet         as IS
import qualified Data.Vector         as V
import qualified Data.List           as L

import           Data.Vector         (Vector)
import           Control.Applicative ((<$>))

import           Data.Maybe

import           Hammer.Math.Algebra
import           Hammer.MicroGraph
import           Hammer.Math.SortSeq
import           Hammer.VTK
import           SubZero

import           VirMat.Core.VoronoiMicro

--import Debug.Trace
--dbg a = trace (">>" ++ show a) a

-- ================================ FlexMicroBuilder =====================================

class FlexMicroBuilder v where
  -- | Stores a flexible microstructure using Subdivision Surfaces. The topology and
  -- the values are store separately in order to allow fast update of values. The topology is
  -- represented by @MicroGraph@.
  data FlexMicro :: * -> * -> *
  -- | This function converts a Voronoi microstructure (convex polygonal geomerty)
  -- to flexible microstructure where arbitrary shape of grains are allowed.
  -- Such a feature is provide by using Subdivision Surfaces.
  mkFlexMicro :: VoronoiMicro v -> FlexMicro v ()
  -- | Render 'FlexMicro' to 'VTK'. The extra info (attributes) are produced by a list of
  -- attribute generators 'RenderGrainProp'. The surfaces are rendered with @n@ levels of
  -- subdivision.
  renderFlexMicro  :: [RenderGrainProp v g] -> Int -> FlexMicro v g -> Vector (VTK v)
  -- | Applies a function to modify the properties of grains
  modifyGrainProps :: (GrainID -> GrainProp a -> b) -> FlexMicro v a -> FlexMicro v b
  -- | List all @GrainID@s
  flexGrainList :: FlexMicro v a -> [GrainID]
  -- | Retrieves a specific grain's property
  flexGrainProp :: GrainID -> FlexMicro v a -> Maybe (GrainProp a)


instance FlexMicroBuilder Vec2 where
  data FlexMicro Vec2 a =
    FlexMicro2D
    { flexGraph2D  :: MicroGraph a (Vector Int) Int () -- ^ Microstructure graph
    , flexPoints2D :: Vector Vec2 -- ^ Controls points (edges and faces)
    } deriving (Show)
  mkFlexMicro vm = getFlexFaces2D vm $ getFlexGrainsAndEdges2D vm
  renderFlexMicro renders n fm@FlexMicro2D{..} = let
    gids    = V.fromList $ getGrainIDList flexGraph2D
    foo gid = renderGrains2D gid renders n fm
    in V.concatMap foo gids
  modifyGrainProps func fm@FlexMicro2D{..} =
    fm { flexGraph2D = flexGraph2D
       { microGrains = HM.mapWithKey foo (microGrains flexGraph2D)}}
    where foo gid gp = setPropValue gp (func gid gp)
  flexGrainList = getGrainIDList . flexGraph2D
  flexGrainProp gid = getGrainProp gid . flexGraph2D

instance FlexMicroBuilder Vec3 where
  data FlexMicro Vec3 a =
    FlexMicro3D
    { flexGraph3D  :: MicroGraph a MeshConn (Vector Int) Int -- ^ Microstructure graph
    , flexPoints3D :: Vector Vec3 -- ^ Controls points (vertices, edges and faces)
    } deriving (Show)
  mkFlexMicro vm = getFlexFaces3D vm $ getFlexEdges3D vm $ getFlexGrainsAndVertices3D vm
  renderFlexMicro renders n fm@FlexMicro3D{..} = let
    gids    = V.fromList $ getGrainIDList flexGraph3D
    foo gid = renderGrains3D gid renders n fm
    in V.concatMap foo gids
  modifyGrainProps func fm@FlexMicro3D{..} =
    fm { flexGraph3D   = flexGraph3D
       { microGrains = HM.mapWithKey foo (microGrains flexGraph3D)}}
    where foo gid gp = setPropValue gp (func gid gp)
  flexGrainList = getGrainIDList . flexGraph3D
  flexGrainProp gid = getGrainProp gid . flexGraph3D

-- =================================================================================

getFlexGrainsAndEdges2D :: VoronoiMicro Vec2 -> FlexMicro Vec2 ()
getFlexGrainsAndEdges2D MicroGraph{..} = let
  vs_clean = filter (hasPropValue . snd) $ HM.toList microEdges
  vh = HM.fromList $ zipWith (\(k, p) i -> (k, setPropValue p i)) vs_clean [0..]
  fp = V.fromList $ mapMaybe (getPropValue . snd) vs_clean
  fg = initMicroGraph { microEdges = vh, microGrains = microGrains }
  in FlexMicro2D fg fp

getFlexFaces2D ::  VoronoiMicro Vec2 -> FlexMicro Vec2 a -> FlexMicro Vec2 a
getFlexFaces2D vm@MicroGraph{..} (FlexMicro2D fm ps) = let
  psSize     = V.length ps
  fs_clean   = fst $ HM.foldlWithKey' foo (V.empty, psSize) microFaces
  getter i m = getPropValue =<< getEdgeProp i m
  foo acc@(vec, n) k p = maybe acc id $ do
    conn <- getPropConn p
    case HS.toList conn of
      [a,b] -> do
        ia <- getter a fm
        ib <- getter b fm
        va <- getter a vm
        vb <- getter b vm
        let prop = setPropValue p $ V.fromList [ia, n, ib]
            avg  = 0.5 *& (va &+ vb)
            v    = (k, avg, prop)
        return (vec `V.snoc` v, n + 1)
      _            -> Nothing
  vv = V.map (\(_,x,_) -> x) fs_clean
  vh = HM.fromList . V.toList $ V.map (\(k,_,p) -> (k,p)) fs_clean
  fg = fm { microFaces = vh }
  fp = ps V.++ vv
  in FlexMicro2D fg fp

-- =================================================================================

getFlexGrainsAndVertices3D :: VoronoiMicro Vec3 -> FlexMicro Vec3 ()
getFlexGrainsAndVertices3D MicroGraph{..} = let
  vs_clean = filter (hasPropValue . snd) $ HM.toList microVertex
  vh = HM.fromList $ zipWith (\(k, p) i -> (k, setPropValue p i)) vs_clean [0..]
  fp = V.fromList $ mapMaybe (getPropValue . snd) vs_clean
  fg = initMicroGraph { microVertex = vh, microGrains = microGrains }
  in FlexMicro3D fg fp

getFlexEdges3D ::  VoronoiMicro Vec3 -> FlexMicro Vec3 a -> FlexMicro Vec3 a
getFlexEdges3D vm@MicroGraph{..} (FlexMicro3D fm ps) = let
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
  in FlexMicro3D fg fp

getFlexFaces3D :: VoronoiMicro Vec3 -> FlexMicro Vec3 a -> FlexMicro Vec3 a
getFlexFaces3D MicroGraph{..} (FlexMicro3D fm ps) = let
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
  in FlexMicro3D fg (ps V.++ fp)

-- =================================================================================

-- | Finds the subdivision mesh for an closed set of subdivision lines
mkMesh :: Vector (Vector Int) -> Int -> Maybe MeshConn
mkMesh es fc = do
  -- check closure of the line's set
  ses <- sortEdges es
  return $ buildMesh (toTS ses) corners
  where
    getTris vec = V.imap (\i x -> (x, vec V.! (i+1), fc)) (V.init vec)
    toTS        = V.toList . V.concatMap getTris
    getCorners v
      -- the emptiness of the inner Vector is checked by 'sortEdges'
      -- and set the ends to be corners
      | V.length v > 0 = IS.fromList [V.head v, V.last v]
      | otherwise      = IS.empty
    cornersSet = V.foldl' (\acc v -> acc `IS.union` getCorners v) IS.empty es
    corners    = IS.toList cornersSet

-- | @faceCenter@ calculates the mass center of a given polygon defined by
-- a array of points and a list of edges (reference to array of points).
-- It expects a non-empty face otherwise a error will rise. DON'T EXPORT ME!!!
faceCenter :: (AbelianGroup v, MultiVec v)=> Vector v -> Vector (Vector Int) -> Maybe v
faceCenter ps vs
  | n > 0     = return $ total &* (1 / fromIntegral n)
  | otherwise = Nothing
  where
    sumBoth (vacu, nacu) x = (vacu &+ sumFecth x, nacu + V.length x)
    sumFecth   = V.foldl' (\acc i -> acc &+ (ps V.! i)) zero
    (total, n) = V.foldl' sumBoth (zero, 0) vs

-- | Sort and check closure of a set of lines (segments)
sortEdges :: (SeqSeg a)=> Vector a -> Maybe (Vector a)
sortEdges = getOneLoop . sortSegs

instance SeqComp Int

instance (SeqComp a)=> SeqSeg (Vector a) where
  type SeqUnit (Vector a) = a
  getSeqHead = V.head
  getSeqTail = V.last
  seqInv     = V.reverse

-- ============================= VTK render for FlexMicro ================================

-- | Creates an attribute generator for each grain in a 'FlexMicro' structure. It needs a
-- name for the generated attribute, that be will identified on VTK visualization software,
-- and a function that generates the attribute itself based on 'GrainID' and 'FlexMicro'.
data RenderGrainProp v g = forall a . RenderElemVTK a =>
                           RenderGrainProp (String, GrainID -> Maybe g -> Maybe a)

-- | Applies an attribute generator to a given VTK (one single grain)
addGrainAttrs :: (RenderElemVTK a, FlexMicroBuilder v)=> GrainID -> FlexMicro v g
              -> [RenderGrainProp v g] -> VTK a -> VTK a
addGrainAttrs gid fm renders vtk = let
  foo (RenderGrainProp (name, func)) = let
    add x v = addCellAttr v (mkCellAttr name (\_ _ _ -> x))
    in maybe id add (func gid (flexGrainProp gid fm >>= getPropValue))
  in L.foldl' (\acc x -> foo x acc) vtk renders

-- | Show the 'GrainID' value in the 'VTK' data.
showGrainID :: RenderGrainProp v g
showGrainID = RenderGrainProp ("GrainID", \gid _ -> return $ unGrainID gid)

-- =================================== Render 2D Grains ==================================

renderGrains2D :: GrainID -> [RenderGrainProp Vec2 g] -> Int -> FlexMicro Vec2 g -> Vector(VTK Vec2)
renderGrains2D gid renders n fm@FlexMicro2D{..} =
  maybe V.empty (V.singleton . addGrainAttrs gid fm renders) $ do
    grainConn  <- getPropConn =<< getGrainProp gid flexGraph2D
    renderGrainBulk2D grainConn n fm

renderGrainBulk2D :: HS.HashSet FaceID -> Int -> FlexMicro Vec2 a -> Maybe (VTK Vec2)
renderGrainBulk2D fs n FlexMicro2D{..} = let
  func acc fid = maybe acc (V.snoc acc) (getPropValue =<< getFaceProp fid flexGraph2D)
  es = HS.foldl' func V.empty fs
  in renderSubOne n flexPoints2D es

-- =================================== Render 3D Grains ==================================

renderGrains3D :: GrainID -> [RenderGrainProp Vec3 g] -> Int -> FlexMicro Vec3 g -> Vector (VTK Vec3)
renderGrains3D gid renders n fm@FlexMicro3D{..}  = let
  grainConn  = getPropConn =<< getGrainProp gid flexGraph3D
  getVTK fid = addGrainAttrs gid fm renders <$> renderFace3D fid n fm
  func acc   = maybe acc (V.snoc acc) . getVTK
  in maybe V.empty (HS.foldl' func V.empty) grainConn

renderFace3D :: FaceID -> Int -> FlexMicro Vec3 a -> Maybe (VTK Vec3)
renderFace3D fid n FlexMicro3D{..} = renderSubTwo n flexPoints3D <$>
                                   (getPropValue =<< getFaceProp fid flexGraph3D)

-- ============================== render subdivision surfaces ============================

-- | Renders a closed set of subdivision lines(faces in 2D grains) as surface(grain's bulk).
renderSubOne :: Int -> Vector Vec2 -> Vector (Vector Int) -> Maybe (VTK Vec2)
renderSubOne n vs es = do
  center <- faceCenter vs es
  mesh   <- mkMesh es (V.length vs)
  let
    sb  = mkSubTwoFromMesh (vs `V.snoc` center) mesh
    sbN = subdivideN n sb
    ps  = V.convert $ subTwoPoints sbN
    ts  = getSubTwoFaces $ subTwoMesh sbN
  return $ mkUGVTK "FlexMicro" ps ts [] []

-- | Renders a subdivision surface(faces in 3D grains).
renderSubTwo :: Int -> Vector Vec3 -> MeshConn -> VTK Vec3
renderSubTwo n vs mesh = let
  sb  = mkSubTwoFromMesh vs mesh
  sbN = subdivideN n sb
  ps  = V.convert $ subTwoPoints sbN
  ts  = getSubTwoFaces $ subTwoMesh sbN
  in mkUGVTK "FlexMicro" ps ts [] []
