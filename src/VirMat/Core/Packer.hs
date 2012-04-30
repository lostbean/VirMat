{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE BangPatterns #-}

module VirMat.Core.Packer where

-- External modules
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.Vector as Vec
import Data.Vector (Vector, (!))
import Data.List (foldl',sortBy)
import Data.Maybe

import Hammer.Math.Vector hiding (Vector)
import qualified Hammer.Math.Vector as AlgLin

import DeUni.DeWall
import DeUni.Types
import DeUni.Dim2.Base2D
import DeUni.Dim3.Base3D



-- TODO generalize for 3D
runPacker::Int -> Box Point2D -> SetPoint Point2D -> IntMap (S2 Point2D) -> (SetPoint Point2D, IntMap (S2 Point2D))
runPacker n box ps wall = let
  pack (!ps0,!ps1,!wall1) i = let
    time      = if i > 12 then 0.08 else 0.06
    ps2       = updateSP box wall1 ps0 ps1 0.6 time
    len2      = Vec.length ps2
    psID2     = [0 .. len2-1]
    (wall2,_) = if i > 60 then (wall1, undefined) else runDelaunay2D box ps2 psID2
    in (ps1,ps2,wall2)
       
  (arrF0, arrF1, wallF1) = foldl' pack (ps, ps, wall) [1..n]
  
  in (arrF1, wallF1)


findPointConn::IntMap (S2 Point2D) -> IntMap IntSet
findPointConn = let
  func acc x = let
    (a, b, c) = face2DPoints x
    add ref to acc = IM.insertWith IS.union ref (IS.singleton to) acc
    in add a b $ add b c $ add c a $
       add b a $ add c b $ add a c acc
  in IM.foldl' func IM.empty

sortGroup::(Ord a) => [(a, b)] -> [(a, [b])]
sortGroup = let
  comp a b = compare (fst a) (fst b)

  groupOrd [] = []
  groupOrd ls@(x:xs) = let
    (store, rest) = span (\a -> fst x == fst a) ls
    in (fst x, map snd store):groupOrd rest
        
  in groupOrd.(sortBy comp)


force::(AlgLin.Vector a, DotProd a, Show a)=> WPoint a -> WPoint a -> a
force ref x = let
  delta    = point x &- point ref
  dir      = normalize delta
  len      = norm delta
  totalR   = sqrt (weigth x) + sqrt (weigth ref)
  freeDist = len - totalR
  choose
    | len == 0               = error "[Packer] Matching WPoint."
    | freeDist >= totalR * 3 = dir &* (3 * totalR)
    | freeDist >= 0          = dir &* (freeDist)
    | freeDist >= -totalR    = dir &* (-10 * freeDist * freeDist)
    | otherwise              = dir &* (-10 * totalR * totalR)
  in choose

evalForce::(AlgLin.Vector a, DotProd a, Show a)=> SetPoint a -> PointPointer -> IntSet -> a
evalForce sp a ns = let
  func acc x = acc &+ force (sp!a) (sp!x)
  in IS.foldl' func zero ns
     
setForce::IntMap (S2 Point2D) -> SetPoint Point2D -> Vector Point2D
setForce tri sp = let
  conn     = findPointConn tri
  update i = let
    f ps = evalForce sp i ps
    in case IM.lookup i conn of 
      Just ps -> f ps
      _       -> zero
  in Vec.generate (Vec.length sp) update 


updateSP::Box Point2D -> IntMap (S2 Point2D) -> SetPoint Point2D -> SetPoint Point2D -> Double -> Double -> SetPoint Point2D
updateSP (Box2D{..}) tri sp0 sp1 damp time = let
  conn     = findPointConn tri
  update i = let 
    p0     = sp0!.i
    p1     = sp1!.i
    new ps = let
      a    = evalForce sp1 i ps
      newP@(Vec2 x y) = ((2 - damp) *& p1) &- ((1-damp) *& p0) &+ (a &* (time*time))
      forceInBox a min max
        | a > max = max
        | a < min = min
        | otherwise = a
      newX = forceInBox x xMin2D xMax2D
      newY = forceInBox y yMin2D yMax2D
      in Vec2 newX newY  
    in case IM.lookup i conn of 
      Just ps -> WPoint {weigth = weigth $ sp1!i, point = new ps}
      _       -> sp1!i
  in Vec.generate (Vec.length sp1) update
     
testForce = map (norm . force (WPoint 1 (Vec2 0 0)) . WPoint 5 . Vec2 0.1)