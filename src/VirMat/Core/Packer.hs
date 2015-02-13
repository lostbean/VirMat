{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}

module VirMat.Core.Packer
       ( runPacker
       , runPacker2D
       , setForce
       , setDisp
       ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

import           Data.IntMap (IntMap)
import           Data.Vector (Vector, (!))
import           Data.List   (foldl',sortBy)
import           Data.IntSet (IntSet)

import           DeUni.DeWall
import           Hammer.Math.Algebra
import           Hammer.VTK

import           Debug.Trace (trace)


runPacker :: (Packer a, Buildable S2 a, Show(Box a))=> Int -> Box a -> SetPoint a
          -> IntMap (S2 a) -> (SetPoint a, IntMap (S2 a))
runPacker n box ps wall = let
  interDeUni = 2
  smooth     = 15

  (_, arrF1, wallF1) = foldl' pack (ps, ps, wall) [1..n]

  pack (!ps0,!ps1,!wall1) i = let
    time      = trace ("time " ++ show i ++ " = ") $
                if i > smooth
                then 0.2
                else 0.2 * (fromIntegral i) / 15
    ps2       = updateSP box wall1 ps0 ps1 0.6 time
    len2      = V.length ps2
    psID2     = [0 .. len2-1]
    (wall2,_) = if (i > smooth) && (rem i interDeUni /= 0)
                then (wall1, undefined)
                else trace "triangulate" $ runDelaunay box ps2 psID2
    in (ps1,ps2,wall2)

  in (arrF1, wallF1)

runPacker2D :: Int -> Box Point2D -> SetPoint Point2D -> IntMap (S2 Point2D)
            -> (SetPoint Point2D, IntMap (S2 Point2D))
runPacker2D n box ps wall = let
  interDeUni = 2
  smooth     = 15

  (_, arrF1, wallF1) = foldl' pack (ps, ps, wall) [1..n]

  pack (!ps0,!ps1,!wall1) i = let
    time      = trace ("time " ++ show i ++ " = ") $
                if i > smooth then 0.2 else 0.2 -- + 0.2 * (1 - (fromIntegral i) / (fromIntegral smooth))
    ps2       = updateSP box wall1 ps0 ps1 0.6 time
    len2      = V.length ps2
    psID2     = [0 .. len2-1]
    (wall2,_) = if (i > smooth) && (rem i interDeUni /= 0)
                then (wall1, undefined)
                else trace "dela" $ runDelaunay box ps2 psID2
    in (ps1,ps2,wall2)

  in (arrF1, wallF1)

sortGroup :: (Ord a) => [(a, b)] -> [(a, [b])]
sortGroup = let
  comp a b = compare (fst a) (fst b)

  groupOrd [] = []
  groupOrd ls@(x:_) = let
    (store, rest) = span (\a -> fst x == fst a) ls
    in (fst x, map snd store):groupOrd rest

  in groupOrd.(sortBy comp)


force :: (Packer a, Show a)=> WPoint a -> WPoint a -> a
force ref x
  | freeDist <= 0 = let
    f = 4 * freeDist
    -- f < 0 if x < 0
    in dir &* (-1*f*f)
  | otherwise     = let
    f = freeDist
    in dir &* f
  where
    -- attraction direction
    delta    = point x &- point ref
    dir      = normalize delta
    ldelta   = norm delta
    totalR   = radius x + radius ref
    freeDist = ldelta - totalR

evalForce :: (Packer a, Show a)=> SetPoint a -> PointPointer -> IntSet -> a
evalForce sp a ns = let
  func acc x = acc &+ force (sp!a) (sp!x)
  f          = IS.foldl' func zero ns
  in f &* (1/(radius $ sp!a))

setForce :: (Packer a)=> IntMap (S2 a) -> SetPoint a -> Vector (Vector a)
setForce tri sp = let
  conn     = findPointConn tri
  update i = case IM.lookup i conn of
    Just ps -> V.map (\n -> force (sp!i) (sp!n)) . V.fromList . IS.toList $ ps  -- evalForce sp i ps
    _       -> V.empty                                                            -- zero
  in V.generate (V.length sp) update

setDisp :: (Packer a)=> IntMap (S2 a) -> SetPoint a -> Vector a
setDisp tri sp = let
  conn     = findPointConn tri
  update i = case IM.lookup i conn of
    Just ps -> getDisplacement sp i ps 0.2
    _       -> zero
  in V.generate (V.length sp) update

getDisplacement :: (Packer a)=> SetPoint a -> Int -> IntSet -> Double -> a
getDisplacement sp i ps time = let
  a    = evalForce sp i ps
  disp = a &* (time*time)
  l    = len disp
  r    = radius $ sp!i
  -- cut-off displacements bigger than 2*r. It avoids multiple sphere
  -- overlaping on the coners due "keepInBox" restriction
  in if l > 0.5*r then disp &* (0.5*r/l) else disp

updateSP :: (Packer a)=> Box a -> IntMap (S2 a) -> SetPoint a -> SetPoint a
         -> Double -> Double -> SetPoint a
updateSP box tri sp0 sp1 damp time = let
  conn     = findPointConn tri
  update i = let
    p0     = sp0!.i
    p1     = sp1!.i
    new ps = let
      -- Verlet integration with damping effect (damp = [0,1])
      deltaPos = getDisplacement sp1 i ps time
      newP     = ((2-damp) *& p1) &- ((1-damp) *& p0) &+ deltaPos
      in keepInBox box newP
    in case IM.lookup i conn of
      Just ps -> (sp1!i) {point = new ps}
      _       -> (sp1!i)
  in V.generate (V.length sp1) update



class (PointND a)=> Packer a where
  findPointConn :: IntMap (S2 a) -> IntMap IntSet
  keepInBox     :: Box a -> a -> a

instance Packer Point2D where
  findPointConn = let
    func acc x = let
      (a, b, c) = face2DPoints x
      add ref to = IM.insertWith IS.union ref (IS.singleton to)
      in add a b $ add b c $ add c a $
         add b a $ add c b $ add a c acc
    in IM.foldl' func IM.empty

  keepInBox Box2D{..} (Vec2 x y) = let
    forceInBox a minA maxA
      | a > maxA = maxA
      | a < minA = minA
      | otherwise = a
    newX = forceInBox x xMin2D xMax2D
    newY = forceInBox y yMin2D yMax2D
    in (Vec2 newX newY)


instance Packer Point3D where
  findPointConn = let
    func acc x = let
      (a, b, c, d) = tetraPoints x
      add ref to = IM.insertWith IS.union ref (IS.singleton to)
      in add a b $ add b c $ add c a $
         add b a $ add c b $ add a c $
         add a d $ add b d $ add c d $
         add d a $ add d b $ add d c acc
    in IM.foldl' func IM.empty

  keepInBox Box3D{..} (Vec3 x y z) = let
    forceInBox a minA maxA
      | a > maxA = maxA
      | a < minA = minA
      | otherwise = a
    newX = forceInBox x xMin3D xMax3D
    newY = forceInBox y yMin3D yMax3D
    newZ = forceInBox z zMin3D zMax3D
    in (Vec3 newX newY newZ)

-- ============================= Testing ================================
testForce :: [Double] -> [Double]
testForce = map (norm . force (WPoint 1 (Vec2 0 0)) . WPoint 5 . Vec2 0.1)

icosahedron :: Double -> Point3D -> (Vector Point3D, Vector (Int, Int, Int))
icosahedron r pos = let
  t = (1 + sqrt 5) / 2   -- golden ratio
  k = 1 / sqrt (1 + t*t) -- correct the radius. For (0, 1, phi) the edge
                         -- is 2 then the radius is sqrt(1+phi*phi)
  points = V.fromList [ (t , 1, 0), (-t, 1,  0), (t, -1, 0), (-t, -1, 0)
                        , (1, 0,  t), (1,  0, -t), (-1, 0, t), (-1, 0, -t)
                        , (0, t,  1), ( 0, -t, 1), (0, t, -1), (0, -t, -1) ]
  tri    = V.fromList [ (0, 8, 4), (0, 5, 10), (2, 4, 9), (2, 11, 5)
                        , (1, 6, 8), (1, 10, 7), (3, 9, 6), (3, 7, 11)
                        , (0, 10, 8), (1, 8, 10), (2, 9, 11), (3, 9, 11)
                        , (4, 2, 0), (5, 0, 2), (6, 1, 3), (7, 3, 1)
                        , (8, 6, 4), (9, 4, 6), (10, 5, 7), (11, 7, 5) ]
  in (V.map ((pos &+) . (r * k *&) . (\(x,y,z) -> Vec3 x y z)) points, tri)

writeWPointsVTKfile :: String -> SetPoint Point3D -> IO ()
writeWPointsVTKfile file points = let
  vtks = V.imap foo points
  foo nid x = let
    (ps, cs) = icosahedron (radius x) (point x)
    psU = V.convert ps
    attr = mkCellAttr "GrainID" (\_ _ _ -> nid)
    in mkUGVTK "WPoint" psU cs [] [attr]
  in writeMultiVTKfile file True vtks
