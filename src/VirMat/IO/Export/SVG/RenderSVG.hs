{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module VirMat.IO.Export.SVG.RenderSVG where

import qualified Data.ByteString.Lazy     as BS
import qualified Data.Vector              as V
import qualified Data.IntMap              as IM
import qualified Data.HashSet             as HS
import qualified Data.List                as L

import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import           Text.Blaze.Html              (Html, unsafeLazyByteString)
import           Data.Vector                  (Vector)
import           Data.IntMap                  (IntMap)

import           DeUni.DeWall
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Data.Maybe
import           Texture.SphereProjection
import           Hammer.Math.Algebra
import           Hammer.MicroGraph
import           Hammer.Math.SortSeq

import           VirMat.Core.VoronoiMicro

import Debug.Trace

-- =======================================================================================

getSizeSpec :: (Maybe Int, Maybe Int) -> SizeSpec2D
getSizeSpec (wth, hht) = case (wth, hht) of
  (Nothing, Nothing) -> Absolute
  (Just w , Nothing) -> Width  (fromIntegral w)
  (Nothing, Just h ) -> Height (fromIntegral h)
  (Just w , Just h ) -> Dims   (fromIntegral w) (fromIntegral h)

renderSVGFile :: String -> SizeSpec2D -> Diagram SVG R2 -> IO ()
renderSVGFile fileName spec dia = let
  build = renderDia SVG (SVGOptions spec Nothing) dia
  in BS.writeFile fileName (renderSvg build)

renderSVGHtml :: SizeSpec2D -> Diagram SVG R2 -> Html
renderSVGHtml spec dia = let
  build = renderDia SVG (SVGOptions spec Nothing) dia
  in unsafeLazyByteString (renderSvg build)

closeUpOnBox :: Box Point2D -> Diagram SVG R2 -> Diagram SVG R2
closeUpOnBox Box2D{..} = let
  r = r2 (xMax2D - xMin2D, yMax2D - yMin2D)
  p = p2 (xMin2D, yMin2D)
  in view p r

renderBox2D :: Box Point2D -> Diagram SVG R2
renderBox2D Box2D{..} = let
  dx = abs (xMax2D - xMin2D)
  dy = abs (yMax2D - yMin2D)
  boxSize = r2 (xMin2D + dx/2, yMin2D + dy/2)
  in rect dx dy
     # translate boxSize
     # lc blue
     # lw 0.05

-- =======================================================================================

getGrainVertices :: VoronoiMicro Vec2 -> [MicroEdge] -> Maybe [Vec2]
getGrainVertices micro mes = let
  getVertex vid = getPropValue =<< getVertexProp vid micro
  foo acc me = case me of
    FullEdge a b -> (a, b):acc
    _            -> acc
  sorted :: Maybe (Vector (VertexID, VertexID))
  sorted = getOneLoop $ sortSegs $ V.fromList $ L.foldl' foo [] mes
  in (mapM (getVertex . snd)) =<< (V.toList <$> sorted)

instance SeqComp VertexID where
  seqComp = (==)

getGrainEdges :: VoronoiMicro Vec2 -> GrainID -> [MicroEdge]
getGrainEdges micro grainID = let
  getFaces gid      = HS.toList <$> (getPropConn =<< getGrainProp gid micro)
  getEdges fid      = HS.toList <$> (getPropConn =<< getFaceProp  fid micro)
  getEdgeValues eid = (getPropConn =<< getEdgeProp  eid micro)
  in case getFaces grainID of
    Nothing   -> []
    Just fids -> let
      eids = concat $ catMaybes (map getEdges fids)
      in catMaybes (map getEdgeValues eids)

renderSetGrain2D :: VoronoiMicro Vec2 -> Diagram SVG R2
renderSetGrain2D micro = let
  gids = getGrainIDList micro
  ps   = map (getGrainVertices micro . getGrainEdges micro) gids
  addGrain acc x = renderGrain2D x <> acc
  in L.foldl' addGrain mempty (catMaybes ps)

renderGrain2D :: [Vec2] -> Diagram SVG R2
renderGrain2D grain = let
  fstp  = head grain
  delta = v2r . head
  func  = fromVertices . map v2p . (++ [fstp])
  in strokeT (func grain)
     # fcA (yellow `withOpacity` 0.15)
     # lw 0.1
     # lc orange
     # translate (delta grain)

-- =======================================================================================

renderSetS2 :: Vector (WPoint Point2D) -> IntMap (S2 Point2D) -> Diagram SVG R2
renderSetS2 ps ss = let
  func acc x = let
    (a,b,c) = face2DPoints x
    in acc <> renderTri (ps!.a) (ps!.b) (ps!.c)
  in IM.foldl func mempty ss

renderSetS2Triangle :: Vector (WPoint Point2D) -> IntMap (S2 Point2D) -> Diagram SVG R2
renderSetS2Triangle ps ss = let
  func acc x = let
    (a,b,c) = face2DPoints x
    in acc <> renderTri (ps!.a) (ps!.b) (ps!.c)
  in IM.foldl func mempty ss

renderSetS2Circle :: IntMap (S2 Point2D) -> Diagram SVG R2
renderSetS2Circle ss = let
  color = green `withOpacity` 0.15
  func acc x = acc <>
               renderCircle (circleCenter x) (sqrt . circleRadius $ x) color
               # fcA (green `withOpacity` 0.15)
  in IM.foldl func mempty ss

renderTri :: Point2D -> Point2D -> Point2D -> Diagram SVG R2
renderTri a b c = let
  ab = v2p a ~~ v2p b
  bc = v2p b ~~ v2p c
  ca = v2p c ~~ v2p a
  tri = ab <> bc <> ca
  in strokeT tri
     # fcA (yellow `withOpacity` 0.30)
     # lw 0.1
     # lc orange
     # translate (v2r a)

renderSetPoint2D :: Vector (WPoint Point2D) -> Diagram SVG R2
renderSetPoint2D ps = let
  rc x = renderCircle (point x) (DeUni.DeWall.radius x) (red `withOpacity` 0.10)
  in V.foldl' (\acc x -> acc <> rc x) mempty ps

v2r :: Vec2 -> R2
v2r (Vec2 x y) = r2 (x,y)

v2p :: Vec2 -> P2
v2p (Vec2 x y) = p2 (x,y)

renderDiffArr :: Vector (WPoint Point2D) ->  Vector (WPoint Point2D) -> Diagram SVG R2
renderDiffArr arr0 arr1 = let
  ds = V.zip arr0 arr1
  in V.foldl (\acc (a,b) -> acc <> renderVector red (point a, point b)) mempty ds

renderForces :: Vector (WPoint Point2D) -> Vector (Vector Point2D) -> Diagram SVG R2
renderForces arr forces = let
  ds = V.zip arr forces
  func x fs = V.foldl (\acc f -> acc <> renderVector green (point x, point x &+ f)) mempty fs
  in V.foldl (\acc (x, fs) -> acc <> func x fs) mempty ds

renderDisp :: Vector (WPoint Point2D) -> Vector Point2D -> Diagram SVG R2
renderDisp arr disp = let
  ds = V.zip arr disp
  in V.foldl (\acc (x, v) -> acc <> renderVector red (point x, point x &+ v)) mempty ds

renderVector :: Colour Double -> (Point2D, Point2D) -> Diagram SVG R2
renderVector color (a,b) = let
  func = v2p
  ab   = func a ~~ func b
  in strokeT ab
   # fcA (color `withOpacity` 0.8)
   # lw 0.2
   # lc color
   # translate (v2r a)

renderCircle :: Point2D -> Double -> AlphaColour Double -> Diagram SVG R2
renderCircle p r color = let
  v = v2r p
  in circle r
     # lc black
     # fcA color
     # translate v
  <> circle 0.2
     # fc black
     # translate v

renderPlot :: [(Double, Double)] -> Diagram SVG R2
renderPlot plot = let
  curve = fromVertices $ map p2 plot
  in strokeT curve
     # translate (r2 $ head plot)
     # lc blue
     # lw 0.01

-- ============================== Sphere Projection ============================
-- TODO add legend
-- | Render a grid for the chosen projection
renderSO3ProjGrid  :: Diagram SVG R2
renderSO3ProjGrid = circle 1 # lc black # lw 0.02

-- | Plot a equaled area pole figure regarding the external frame reference (X,Y,Z or
-- ND,TD,RD). Plots both half-spheres overlapped.
renderProjPoint  :: Vec2 -> Diagram SVG R2
renderProjPoint p = circle (0.01)
                    # lw 0
                    # lcA (red `withOpacity` 0.3)
                    # fcA (red `withOpacity` 0.3)
                    # translate (v2r p)

renderStereoPoleFigure :: Vector Normal3 -> Diagram SVG R2
renderStereoPoleFigure = let
  foo = V.foldl' (\acc n -> acc <> renderProjPoint n) mempty
  in (renderSO3ProjGrid <>) . foo . getBothProj . V.map steroSO3Proj

renderLambertPoleFigure :: Vector Normal3 -> Diagram SVG R2
renderLambertPoleFigure = let
  foo = V.foldl' (\acc n -> acc <> renderProjPoint n) mempty
  in (renderSO3ProjGrid <>) . foo . getBothProj . V.map lambertSO3Proj

-- ============================== Histogram  ============================

renderHistogram :: Double -> Double -> Double -> [Double] -> Diagram SVG R2
renderHistogram initial final step yAxe = let
  xAxe  = [initial, initial + step .. final]
  bar x y
    | y == 0    = mempty
    | otherwise = let
      v = r2 (x, y/2)
      in rect step y
         # lc black
         # fcA (blue `withOpacity` 0.8)
         # translate v
  in mconcat $ zipWith bar xAxe yAxe

renderHistogram' :: Double -> Double -> [Double] -> Diagram SVG R2
renderHistogram' initial final yAxe = let
  n :: Double
  n = fromIntegral . length $ yAxe
  nBinRef :: Int
  nBinRef = floor $ sqrt n
  step  = 0.1 * (final - initial) / (fromIntegral nBinRef)
  xAxe  = [initial, initial + step .. final]
  bar x y
    | y == 0    = mempty
    | otherwise = let
      v = r2 (x, y/2)
      in rect step y
         # lc black
         # fcA (blue `withOpacity` 0.8)
         # translate v
  in mconcat $ zipWith bar xAxe yAxe
