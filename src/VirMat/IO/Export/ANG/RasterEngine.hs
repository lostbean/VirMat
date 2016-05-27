{-# LANGUAGE RecordWildCards #-}
module VirMat.IO.Export.ANG.RasterEngine
  ( rasterTriangle
  , rasterTriangleFaster
  , flexmicroToANG
  , getGrainMeshAndProp
  , isInsideTriangle
  ) where

import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Data.List
import Data.Function
import DeUni.Dim2.Base2D
import DeUni.Types
import File.ANGReader
import Hammer.MicroGraph
import Linear.Vect
import Texture.Orientation
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as VM
import qualified Data.HashSet         as HS

import VirMat.Core.FlexMicro

-- =============================== tools for ANG's grid ==================================

toGrid :: ANGgrid -> (Double, Double) -> (Int, Int)
toGrid g (x, y) = (i, j)
  where
    i = round $ (x - xmin) / stepx
    j = round $ (y - ymin) / stepy
    (stepx, stepy)  = xystep g
    (xmin, ymin, _) = origin g

fromGrid :: ANGgrid -> (Int, Int) -> (Double, Double)
fromGrid g (i, j) = (x, y)
  where
    x = fromIntegral i * stepx + xmin
    y = fromIntegral j * stepy + ymin
    (stepx, stepy)  = xystep g
    (xmin, ymin, _) = origin g

-- | Get the linear position of a given node in the ANG square mesh.
toLinPos :: ANGgrid -> (Int, Int) -> Int
toLinPos g = \(y, x) -> y * nx + x
  where (_, nx, _) = rowCols g

-- | Calculate a square grid with same step size in both directions.
calculateGrid :: Double -> Box Vec2 -> ANGgrid
calculateGrid step box = let
  dx = xMax2D box - xMin2D box
  dy = yMax2D box - yMin2D box
  nx = ceiling $ abs (dx / step)
  ny = ceiling $ abs (dy / step)
  in ANGgrid
     { rowCols = (ny, nx, nx)
     , xystep  = (step, step)
     , origin  = (xMin2D box, yMin2D box, 0)
     , hexGrid = False
     }

-- ================================= Fast rasterization ==================================

getSegSlope :: Vec2D -> Vec2D -> Double
getSegSlope (Vec2 v1x v1y) (Vec2 v2x v2y) = (v2x - v1x) / (v2y - v1y)

-- | Fill row between two points.
fillRow :: ANGgrid -> Int -> Double -> Double -> [(Int, Int)]
fillRow g yline xstart xfinal = let
  xs = fst $ toGrid g (xstart, undefined)
  xf = fst $ toGrid g (xfinal, undefined)
  in [(x, yline) | x <- [min xs xf .. max xs xf]]

-- | Get range of row (starting row, number of rows)
getRowsRange :: ANGgrid -> Vec2D -> Vec2D -> (Int, Int)
getRowsRange g (Vec2 _ v1y) (Vec2 _ v2y) = (ya, abs $ yb - ya)
  where
    ya = snd $ toGrid g (undefined, v1y)
    yb = snd $ toGrid g (undefined, v2y)

fillBottomFlatTriangle :: ANGgrid -> Vec2D -> Vec2D -> Vec2D -> [(Int, Int)]
fillBottomFlatTriangle g p1 p2 p3 = let
  m1 = getSegSlope p1 p2
  m2 = getSegSlope p1 p3
  (ya, n) = getRowsRange g p1 p2
  v1x = _1 p1
  func i = fillRow g (ya+i) (v1x + m1* fromIntegral i) (v1x + m2 * fromIntegral i)
  in concatMap func [0 .. n]

fillTopFlatTriangle :: ANGgrid -> Vec2D -> Vec2D -> Vec2D -> [(Int, Int)]
fillTopFlatTriangle g p1 p2 p3 = let
  m1 = getSegSlope p3 p1
  m2 = getSegSlope p3 p2
  (ya, n) = getRowsRange g p3 p1
  v3x = _1 p3
  func i = fillRow g (ya-i) (v3x + m1 * fromIntegral i) (v3x + m2 * fromIntegral i)
  in concatMap func [0 .. n]

-- | Fast rasterization algorithm for filled triangles. Splits the triangle, if it is
-- necessary, in two: one bottom-flat and one top-flat. Then fill both triangles row-by-row.
rasterTriangleFaster :: ANGgrid -> Vec2D -> Vec2D -> Vec2D -> [(Int, Int)]
rasterTriangleFaster g p1 p2 p3
  -- in case of bottomflat triangle
  | v2y == v3y = fillBottomFlatTriangle g v1 v2 v3
  -- in case of topflat triangle
  | v1y == v2y = fillTopFlatTriangle g v1 v2 v3
  | otherwise = let
    -- split the triangle in a topflat and bottomflat
    v4 = Vec2 (v1x + ((v2y - v1y) / (v3y - v1y)) * (v3x - v1x)) v2y
    in fillBottomFlatTriangle g v1 v2 v4
       ++ fillTopFlatTriangle g v2 v4 v3
  where
    -- sort the vertices by y-coordinate, the v1 is the topmost vertice
    [v1, v2, v3] = sortBy (compare `on` _2) [p1, p2, p3]
    Vec2 v1x v1y = v1
    Vec2 _   v2y = v2
    Vec2 v3x v3y = v3

-- ================================ Simple rasterization =================================

boundingBox :: ANGgrid -> Vec2D -> Vec2D -> Vec2D -> ((Int, Int), (Int, Int))
boundingBox g (Vec2 ax ay) (Vec2 bx by) (Vec2 cx cy) = let
  ur = toGrid g (max (max ax bx) cx, max (max ay by) cy)
  ll = toGrid g (min (min ax bx) cx, min (min ay by) cy)
  in (ll, ur)

crossProduct :: Vec2D -> Vec2D -> Double
crossProduct (Vec2 ax ay) (Vec2 bx by) = ax*by - ay*bx

-- | Simple algorithm for filled triangle's rasterization but the not the fastest. Test
-- which nodes are inside the given triangle. Optimized by testing only the nodes from an
-- bounding box. Note that ratio area (triangle) / area (box) determines the efficiency of
-- this algorithm and the best case is 0.5 (50%).
rasterTriangle :: ANGgrid -> Vec2D -> Vec2D -> Vec2D -> [(Int, Int)]
rasterTriangle g p1 p2 p3 = let
  ((llx,lly), (urx, ury)) = boundingBox g p1 p2 p3
  mesh = [(i, j) | i <- [llx..urx], j <- [lly..ury]]
  -- using memorization of "isInsideTriangle p1 p2 p3"
  func = isInsideTriangle (p1, p2, p3) . mkVec2 . fromGrid g
  in filter func mesh

-- | Test if point is inside the triangle. It has memorization on the first parameter
-- (triangle) therefore call it with partial application.
isInsideTriangle :: (Vec2D, Vec2D, Vec2D) -> Vec2D -> Bool
isInsideTriangle (p1, p2, p3 )= let
  -- memorization
  vs1 = p2 &- p1
  vs2 = p3 &- p1
  k   = crossProduct vs1 vs2
  in \x -> let
    p = x &- p1
    s = crossProduct p vs2 / k
    t = crossProduct vs1 p / k
    -- is inside triangle?
    in (s >= 0) && (t >= 0) && (s + t <= 1)

-- ================================= ANG Construction ====================================

-- | Information associated to each point. For futher reference consult OIM manual.
mkPoint :: Quaternion -> Double -> (Int, Int) -> Int -> ANGpoint
mkPoint q cindex (x, y) ph = ANGpoint
  { rotation = q
  , xpos     = x
  , ypos     = y
  , iq       = 100
  , ci       = cindex
  , phaseNum = ph
  , detecInt = 1
  , fit      = 1
  }

-- | Information describing the measuriment.
einfo :: ANGinfo
einfo = ANGinfo
  { workDist = 16       -- Double
  , pixperum = 1        -- Double
  , operator = "VirMat" -- String
  , sampleID = "007"    -- String
  , scanID   = "666"    -- String
  }

feAlpha :: ANGphase
feAlpha = ANGphase
  { phase        = 1
  , materialName = "Iron (Alpha)"
  , formula      = "Fe"
  , info         = ""
  , symmetry     = "43"
  , latticeCons  = (2.87, 2.87, 2.87, 90, 90, 90)
  , numFamilies  = 0              -- Int
  , hklFamilies  = []             -- [(Int, Int, Int, Int, Double, Int)]
  , elasticConst = []             -- [(Double, Double, Double, Double, Double, Double)]
  , categories   = (0,0,0,0,0)
  }

mkBackground :: ANGgrid -> V.Vector ANGpoint
mkBackground g = let
  (ny, nx, _) = rowCols g
  toXY i = let (y, x) = i `quotRem` ny in (x, y)
  nullPoint (x, y) = ANGpoint
    { rotation = zerorot
    , xpos     = x
    , ypos     = y
    , iq       = 100
    , ci       = -1
    , phaseNum = 0
    , detecInt = 1
    , fit      = 1
    }
  in V.generate (ny*nx) (nullPoint . toXY)

-- | Hold the whole ANG data strcuture
angInit :: Double -> Box Vec2 -> ANGdata
angInit step box = let
  ginfo = calculateGrid step box
  in ANGdata
     { nodes    = mkBackground ginfo
     , grid     = ginfo
     , ebsdInfo = einfo
     , phases   = [feAlpha]
     }

-- ================================== Raster FlexMicro ===================================

getGrainMesh :: HS.HashSet FaceID -> Int -> FlexMicro Vec2 a -> Maybe (Vector Vec2D, Vector (Int, Int, Int))
getGrainMesh fs n FlexMicro2D{..} = let
  func acc fid = maybe acc (V.snoc acc) (getPropValue =<< getFaceProp fid flexGraph2D)
  es = HS.foldl' func V.empty fs
  in getSubOneTriangulation n flexPoints2D es

-- | Extract the grain's value and its triangular mesh with a given level of subdivision.
-- INPUT: grain ID, level of mesh subdivision, microstructure. OUTPUT: (nodes, triangles, property)  
getGrainMeshAndProp :: GrainID -> Int -> FlexMicro Vec2 g -> Maybe (Vector Vec2D, Vector (Int, Int, Int), g)
getGrainMeshAndProp gid n fm@FlexMicro2D{..} = do
  (prop, grainConn) <- getPropBoth =<< getGrainProp gid flexGraph2D
  (ps, ts)          <- getGrainMesh grainConn n fm
  return (ps, ts, prop)

-- | Transform a microstructure to ANG using raster algorithm. INPUT: level of subdivision,
-- step size, bounding box, microstructure. OUTPUT: ANG data structure
flexmicroToANG :: Int -> Double -> Box Vec2 -> FlexMicro Vec2 Quaternion -> ANGdata
flexmicroToANG n step box fm@FlexMicro2D{..} = let
  a0 = angInit step box
  gs = getGrainIDList flexGraph2D
  toPos = toLinPos (grid a0)
  func m gid = let
    (ps, ts, q) = fromMaybe (V.empty, V.empty, zerorot) (getGrainMeshAndProp gid n fm)
    fillTriangle (p1, p2, p3) = let
      xys = rasterTriangle (grid a0) (ps V.! p1) (ps V.! p2) (ps V.! p3)
      in mapM_ (\xy -> let pos = toPos xy in VM.write m pos (mkPoint q 1 xy 1)) xys
    in V.mapM_ fillTriangle ts
  in a0 {nodes = V.modify (\m -> mapM_ (func m) gs) (nodes a0)}

-- ====================================== testing ========================================

test :: ANGdata
test = let
  step = 0.5
  box  = Box2D {xMax2D = 10, xMin2D = 0, yMax2D = 10, yMin2D = 0}

  a0 = angInit step box

  xys = rasterTriangle (grid a0) (Vec2 1 1.2) (Vec2 1.4 8) (Vec2 7.1 8.1)
  func m = mapM_ (\xy -> let
                         pos = toLinPos (grid a0) xy
                         in VM.write m pos (mkPoint zerorot 1 xy 1)
                 ) xys
  in a0 {nodes = V.modify func (nodes a0)}
