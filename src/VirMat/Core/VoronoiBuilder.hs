{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module VirMat.Core.VoronoiBuilder
( convertDT2Voronoi
, findGrainsTree
, Level1(..)
, Level2(..)
, VoronoiGrain
, Grain
, grainCenter
, faces
, VoronoiFace
, faceTo
, edges
) where

-- External modules
import qualified Data.List as L
import Data.List (null, foldl', sortBy)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap, mapWithKey)
import qualified Data.Vector as Vec
import Data.Vector (Vector, (!))
import Data.Maybe
import Control.Monad (liftM)

import Hammer.Math.Vector hiding (Vector)

import DeUni.Dim3.Base3D
import DeUni.Dim2.Base2D
import DeUni.Types

import Debug.Trace (trace)

-- | Main Fucntion
convertDT2Voronoi sP = makeGrain sP . findGrainsTree

class (PointND a)=> VoronoiGrainBuilder a where
  type MaxLevel a :: *
  type Grain    a :: *
  foldS2          :: (PointPointer -> b -> b) -> b -> S2 a -> b
  findGrainsTree  :: IntMap (S2 a) -> [MaxLevel a]
  makeGrain       :: SetPoint a -> [MaxLevel a] -> [Grain a]
  
instance VoronoiGrainBuilder Point3D where
  type MaxLevel Point3D = Level2 Point3D
  type Grain    Point3D = VoronoiGrain Point3D
  
  foldS2 func acc s2 = let         
    (a, b, c, d) = tetraPoints s2
    in func a $ func b $ func c $ func d acc
  
  findGrainsTree = level1to2 IS.empty . calcLevelOne IS.empty
  makeGrain sP   = mapMaybe (buildGrain sP)

instance VoronoiGrainBuilder Point2D where
  type MaxLevel Point2D = Level1 Point2D
  type Grain    Point2D = VoronoiFace Point2D
  
  foldS2 func acc s2 = let         
    (a, b, c) = face2DPoints s2
    in func a $ func b $ func c acc
  
  findGrainsTree = calcLevelOne IS.empty
  -- Assume PointPointer >= 0
  makeGrain sP   = mapMaybe (buildFace (-1) sP)


type SetPointer = IntSet

-- | Recursive data tree for constructe grain structure.
-- L3 (level three) - Simplex with a commun vertex, such a vertex is the center of a grain.
-- L2 (level two) - Simplex with a commun vertex - vertex pair, such a a pair defines a face.
data Level1 a = L1 PointPointer (IntMap (S2 a))
data Level2 a = L2 PointPointer [Level1 a]
   
instance (Show (Level1 a))=> Show (Level2 a) where
  show (L2 id x) = "\n L2: "  ++ (show id) ++ (show x)
instance (PointND a, Show (S2 a))=> Show (Level1 a) where
  show (L1 id x) = "\n\t L1: " ++ (show id) ++ (IM.foldl (\acc a -> acc ++
                   "\n\t\t " ++ show a) [] x)

findNode::(VoronoiGrainBuilder a)=> SetPointer -> IntMap (S2 a) -> IntMap (IntMap (S2 a))
findNode blacklist = IM.foldlWithKey func IM.empty
  where
    func acc s2ID s2 =
      let        
        addS2 new old = IM.insert s2ID s2 old
        
        add pointPointer im
          | IS.member pointPointer blacklist = im
          | otherwise = IM.insertWith addS2 pointPointer (IM.singleton s2ID s2) im
      in foldS2 add acc s2

calcLevelOne::(VoronoiGrainBuilder a)=> SetPointer -> IntMap (S2 a) -> [Level1 a]
calcLevelOne blacklist = IM.foldlWithKey (\acc id ls -> (L1 id ls):acc) [] . findNode blacklist
    
level1to2::(VoronoiGrainBuilder a)=> SetPointer -> [Level1 a] -> [Level2 a]
level1to2 blacklist = map (\(L1 id x) -> L2 id (calcLevelOne (IS.insert id blacklist) x))









data VoronoiGrain a = VoronoiGrain
   { grainCenter   :: a
   , grainCenterIx :: PointPointer
   , faces         :: [VoronoiFace a]
   }

data VoronoiFace a = VoronoiFace
   { faceTo   :: a
   , faceToIx :: PointPointer
   , edges    :: [(Int, S2 a)]
   }


instance (Show (VoronoiFace a))=> Show (VoronoiGrain a) where
  show g = let
     showFaces = ("\n\t\t|- " ++) . show
     in "Grain " ++ show (grainCenterIx g) ++ " :" ++ concatMap showFaces (faces g) ++ "\n" 
instance Show (VoronoiFace Point3D) where
  show f = "Face to grain " ++ show (faceToIx f) ++ " -> " ++ (show $ map (tetraPoints.snd) (edges f)) 

buildGrain::(VoronoiGrainBuilder a)=> SetPoint a -> Level2 a -> Maybe (VoronoiGrain a)
buildGrain sP (L2 id fs) = let
  buildGrain = return . VoronoiGrain (sP!.id) id
  sequence []     = Just []
  sequence (x:xs) = case buildFace id sP x of
    Just f -> sequence xs >>= return . (f:)
    _      -> Nothing
  in sequence fs >>= buildGrain

buildFace::(VoronoiGrainBuilder a)=> PointPointer -> SetPoint a -> Level1 a -> Maybe (VoronoiFace a)
buildFace id1 sP (L1 id2 rc) = let
  makeFace = liftM (VoronoiFace (sP!.id2) id2)
  --TODO rm toList
  s = seqNode (id1, id2) (IM.toList rc)
  in makeFace s

seqNode::(VoronoiGrainBuilder a)=> (PointPointer, PointPointer) -> [(Int, S2 a)] -> Maybe [(Int, S2 a)]
seqNode  _       []      = Nothing
seqNode  _       [_]     = Nothing
seqNode (r1,r2) (s2:s2s) = sequence (fst seed) s2s >>= return . (s2:)
  where
    isIn ref = not . null . findVertex (==ref)
    seed     = case findVertex (\_ -> True) s2 of
      [a,b] -> (a,b)
      _     -> error "[VoronoiGrainBuilder] Bad triangulation!"
    
    findVertex func (_, s2) = let
      add key ks
        | key == r1 || key == r2 = ks
        | func key  = key:ks
        | otherwise = ks
      in foldS2 add [] s2

    isClosure = isIn (snd seed)

    sequence refPP [] = Nothing
    sequence refPP xs = case getNext refPP xs of
      Just (nextS2, list) ->
        if null list
        then
          if isClosure nextS2 then Just [nextS2] else Nothing
        else
          let nextPP = head $ findVertex (/=refPP) nextS2
          in sequence nextPP list >>= return . (nextS2:)
      _                 -> Nothing
      
    getNext ref xs = case break (isIn ref) xs of
      ([], [])   -> Nothing
      (_ , [])   -> Nothing
      (as, b:bs) -> Just (b, as ++ bs)

          

