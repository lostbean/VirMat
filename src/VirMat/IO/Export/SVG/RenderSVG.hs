{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.IO.Export.SVG.RenderSVG where

import qualified Data.ByteString.Lazy     as BS
import qualified Blaze.ByteString.Builder as B
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Text.Blaze (Html, lazyText, unsafeLazyByteString)
import Data.Colour (AlphaColour,withOpacity)

import qualified Data.List as L
import qualified Data.Vector as Vec
import qualified Data.IntMap as IM
import Data.Vector (Vector)
import Data.IntMap (IntMap)

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.SVG

import DeUni.Types
import DeUni.Dim2.Base2D
import Hammer.Math.Vector hiding (Vector)

import VirMat.Core.VoronoiBuilder

sizeSpec (width, height) = case (width, height) of
  (Nothing, Nothing) -> Absolute
  (Just w, Nothing)  -> Width (fromIntegral w)
  (Nothing, Just h)  -> Height (fromIntegral h)
  (Just w, Just h)   -> Dims (fromIntegral w) (fromIntegral h)

renderSVGFile :: String -> SizeSpec2D -> Diagram SVG R2 -> IO ()
renderSVGFile fileName sizeSpec dia = let
  build = renderDia SVG (SVGOptions fileName sizeSpec) dia
  in BS.writeFile fileName (renderSvg build)

renderSVGHtml :: SizeSpec2D -> Diagram SVG R2 -> Html
renderSVGHtml sizeSpec dia = let
  build = renderDia SVG (SVGOptions "" sizeSpec) dia
  in unsafeLazyByteString (renderSvg build)

closeUpOnBox::Box Point2D -> Diagram SVG R2 -> Diagram SVG R2
closeUpOnBox Box2D{..} = let
  r = r2 (xMax2D - xMin2D, yMax2D - yMin2D)
  p = p2 (xMin2D, yMin2D)
  in view p r

renderBox2D::Box Point2D -> Diagram SVG R2
renderBox2D Box2D{..} = let
  dx = abs (xMax2D - xMin2D)
  dy = abs (yMax2D - yMin2D)
  boxSize = r2 (xMin2D + dx/2, yMin2D + dy/2)
  in rect dx dy
     # translate boxSize
     # lc blue
     # lw 0.05

renderSetGrain2D::[VoronoiFace Point2D] -> Diagram SVG R2
renderSetGrain2D = L.foldl'(\acc x -> renderGrain2D x <> acc) mempty 

renderGrain2D::VoronoiFace Point2D -> Diagram SVG R2
renderGrain2D grain = let
  delta = v2r . circleCenter . snd . head . edges
  func  = fromVertices . map (v2p . circleCenter . snd) . (\x -> x ++ [head x]) . edges
  in strokeT (func grain)
     # fcA (yellow `withOpacity` 0.15)
     # lw 0.1
     # lc orange
     # translate (delta grain)

renderSetS2::Vector (WPoint Point2D) -> IntMap (S2 Point2D) -> Diagram SVG R2
renderSetS2 ps ss = let
  func acc x = let
    (a,b,c) = face2DPoints x
    in acc <> renderTri (ps!.a) (ps!.b) (ps!.c)
  in IM.foldl func mempty ss
     
renderSetS2Triangle::Vector (WPoint Point2D) -> IntMap (S2 Point2D) -> Diagram SVG R2
renderSetS2Triangle ps ss = let
  func acc x = let
    (a,b,c) = face2DPoints x
    in acc <> renderTri (ps!.a) (ps!.b) (ps!.c)  
  in IM.foldl func mempty ss

renderSetS2Circle::IntMap (S2 Point2D) -> Diagram SVG R2
renderSetS2Circle ss = let
  func acc x = acc <> renderCircle (circleCenter x) (sqrt . circleRadius $ x) (green `withOpacity` 0.15)
                    # fcA (green `withOpacity` 0.15)
  in IM.foldl func mempty ss

renderTri::Point2D -> Point2D -> Point2D -> Diagram SVG R2
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

renderSetPoint2D::Vector (WPoint Point2D) -> Diagram SVG R2
renderSetPoint2D ps = Vec.foldl' (\acc x -> acc <> renderCircle (point x) (sqrt . weigth $ x) (red `withOpacity` 0.10)) mempty ps

v2r (Vec2 x y) = r2 (x,y)

v2p (Vec2 x y) = p2 (x,y)

renderDiffArr::Vector (WPoint Point2D) ->  Vector (WPoint Point2D) -> Diagram SVG R2
renderDiffArr arr0 arr1 = let
  ds = Vec.zip arr0 arr1
  in Vec.foldl (\acc (a,b) -> acc <> renderVector (point a, point b)) mempty ds

renderForces::Vector (WPoint Point2D) -> Vector Point2D -> Diagram SVG R2
renderForces arr forces = let
  ds = Vec.zip arr forces
  in Vec.foldl (\acc (x,f) -> acc <> renderVector (point x, point x &+ (f &* 0.1))) mempty ds

renderVector::(Point2D, Point2D) -> Diagram SVG R2
renderVector (a,b) = let
  func = v2p
  ab   = func a ~~ func b
  in strokeT ab
   # fcA (green `withOpacity` 0.8)
   # lw 0.2
   # lc green
   # translate (v2r a)

renderCircle::Point2D -> Double -> AlphaColour Double -> Diagram SVG R2
renderCircle point diameter color = let
  v = v2r point                      
  in circle diameter
     # lc black
     # fcA color
     # translate v 
  <> circle 0.2
     # fc black
     # translate v
     




renderPlot::[(Double, Double)] -> Diagram SVG R2
renderPlot plot = let
  curve = fromVertices $ map p2 plot
  in strokeT curve
     # translate (r2 $ head plot)
     # lc blue
     # lw 0.01


renderHistogram::Double -> Double -> Double -> [Double] -> Diagram SVG R2
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

renderHistogram'::Double -> Double -> [Double] -> Diagram SVG R2
renderHistogram' initial final yAxe = let
  max = L.maximum yAxe
  min = L.minimum yAxe
  step  = 0.1 * (final - initial) / (fromIntegral . floor . sqrt . fromIntegral . length $ yAxe)
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