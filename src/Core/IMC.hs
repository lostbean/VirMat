
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Core.IMC
( imc
) where


-- %%%%%%%%%% External dependences %%%%%%%%%%%
import Control.Monad (liftM)
import Control.Monad (replicateM, liftM, foldM)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.ST (runST)
import Data.Array.Diff (listArray, (!), elems, bounds, (//), DiffArray)
import Data.IORef
import Data.IntMap (IntMap)
import Data.Random
import Data.Random.RVar
import Data.Random.Source.StdGen
import Data.Vec (Vec3D, vec, unpack, pack, norm)
import System.Random.Mersenne.Pure64
import qualified Data.IntMap as IM
import qualified Data.Set as S

-- %%%%%%%%%% Internal dependences %%%%%%%%%%%
import Math.DeUni ( Box(..)
                  , isInBox
                  , runDeWall
                  , reRunDeWall  
                  , extractAllSimplexFaces
                  , Simplex (..)
                  , Face (..)
                  , Point
                  , PointPointer (..)
                  , SetPoint
                  , SetSimplex
                  , SetActiveSubUnits
                  , ActiveSubUnit (..)
                  , StateVarsMBC (externalFaces) )
import Distributions.GrainSize.DelaunayStatistics (calcStat, printToFile, makeDistFunc, grainSize, DistributionFunc (..))
import Distributions.GrainSize.GrainDistributionGenerator (DistributedPoints(..), getWavePointList, getRandomGen)
import Douane.Export.VTK.VTKGenSimplex
import Douane.Import.Prompt.CommandLineInput (RandomSeed(..))




import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s) x




imc::DistributedPoints -> IORef PureMT -> DistributionFunc -> IO (SetSimplex, StateVarsMBC Face)
imc dP gen distFunc = do
  let 
    (lb,hb)        = bounds (setPoint dP)
    ps             = [lb..hb]
    (wall, wallSt) = runDeWall (box dP) (setPoint dP) ps
  case calcStat (setPoint dP) wall grainSize distFunc of
    Just (err,_) -> func dP wall wallSt err
    _            -> return (IM.empty, wallSt)
  
  where
    func dP wall wallst refErr = do
      pointer          <- getAPointer dP gen
      (rWall, rWallST) <- removeBrick dP wall wallst pointer
      (newP, newDP)    <- tossTheDiceFull dP gen pointer
      (aWall, aWallST) <- addBrick newDP rWall rWallST pointer
      case calcStat (setPoint newDP) aWall grainSize distFunc of
        Just (err, stat) -> recursion 
      -- if and $ map (testProperTetrahedron (setPoint newDP) pst) (IM.elems aWall) then ze else error "Ah nao!"
          where
            (lbt,hbt)        = bounds (setPoint newDP)
            pst              = [lbt..hbt]
            name = take 15 (show err) ++ dropWhile (/= 'e')  (show err)
            recursion
              | err < 1e-3       = return (aWall, aWallST)
              | err > refErr     = func dP wall wallst refErr
              | err <= refErr    = do
                print ("err: " ++ show refErr)
                printToFile ("stat"++name++".txt") stat 
                func newDP aWall aWallST err
        Nothing -> return (IM.empty, wallst)


removeBrick dP wall wallST pointer = do
  --print "remove..."
  --writeVTKfile ("rdel"++show pointer++".vtu") (setPoint dP) simp
  --writeVTKfile ("radd"++show pointer++".vtu") (setPoint dP) ws
  --writeVTKfile ("r"++show pointer++".vtu") (setPoint dP) cleanWall
  return (IM.union cleanWall ws, nWallST)
  where
    sP            = setPoint dP
    simp          = simplexVertexFinder wall pointer
    cleanWall     = IM.difference wall simp
    setFace       = updateActivefaces sP simp (externalFaces wallST)
    wallST'       = wallST {externalFaces = S.empty}
    ps            = getPoints setFace
    (ws, nWallST) = reRunDeWall wallST' (box dP) sP ps setFace
      
addBrick dP wall wallST pointer = do
  --print "add..."
  --writeVTKfile ("adel"++show pointer++".vtu") (setPoint dP) simp
  --writeVTKfile ("aadd"++show pointer++".vtu") (setPoint dP) ws
  --writeVTKfile ("a"++show pointer++".vtu") (setPoint dP) cleanWall  
  return (IM.union cleanWall ws, nWallST)
  where
    sP            = setPoint dP
    newP          = sP!pointer
    simp          = simplexFinder wall newP
    cleanWall     = IM.difference wall simp
    setFace       = updateActivefaces sP simp (externalFaces wallST)
    wallST'       = wallST {externalFaces = S.empty}
    ps            = pointer:(getPoints setFace)
    (ws, nWallST) = reRunDeWall wallST' (box dP) sP ps setFace

getPoints::SetActiveSubUnits Face -> [PointPointer]
getPoints faces = S.elems $ S.fold func S.empty faces
  where
    func x set =
      let (a,b,c) = (facePoints.activeUnit) x 
      in S.insert a $ S.insert b $ S.insert c set

getAPointer::DistributedPoints -> IORef PureMT -> IO PointPointer
getAPointer dp gen =
  let (PointPointer lb, PointPointer hb) = bounds (setPoint dp)
  in liftM PointPointer $ sampleFrom gen (uniform lb hb)
  

simplexVertexFinder::SetSimplex -> PointPointer -> SetSimplex
simplexVertexFinder sS p = IM.filter func sS
  where
    func = isThere.setCellID          
    isThere (a,b,c,d) = a == p || b == p || c == p || d == p

simplexFinder::SetSimplex -> Point -> SetSimplex 
simplexFinder sS p = IM.filter func sS
  where
    func x = isInSphere (circumSphereCenter x) (circumRadius x)
    isInSphere c r = r >= (norm $ p - c)

updateActivefaces::SetPoint -> SetSimplex -> SetActiveSubUnits Face -> SetActiveSubUnits Face
updateActivefaces sP simps actSet = foldl updateSubUnit actSet faces
  where
    faces        = concatMap getActFaces (IM.elems simps)
    getActFaces  = (map invActUnit).(extractAllSimplexFaces sP)
    invFace x    = x { refND = (inv.refND) x }
    invActUnit x = x { activeUnit = (invFace.activeUnit) x, assocND = (inv.assocND) x }
    inv::Vec3D -> Vec3D
    inv = (*) (pack $ vec (-1))
    updateSubUnit set actFace = 
      case S.member actFace set of
        False -> S.insert actFace set
        True  -> S.delete actFace set

tossTheDice::DistributedPoints -> IORef PureMT -> PointPointer -> IO (Maybe (Point, DistributedPoints))
tossTheDice dp gen ix = do
  let vec = boxFrac (box dp) 0.1
      b   = box dp
      arr = setPoint dp
  delta  <- getWavePointList gen stdUniform vec
  let p      = (arr!ix + delta)
      newArr = (setPoint dp) // [(ix, p)]
  if isInBox b p
    then return $ Just (p, dp { setPoint = newArr })
    else return Nothing
  
  
boxFrac::Box -> Double -> (Double, Double, Double)  
boxFrac box frac = (dx*frac, dy*frac, dz*frac)  
  where
    dx = (xMax box - xMin box)
    dy = (yMax box - yMin box)
    dz = (zMax box - zMin box)


tossTheDiceFull::DistributedPoints -> IORef PureMT -> PointPointer -> IO (Point, DistributedPoints)
tossTheDiceFull dp gen ix = do
  let vec  = boxFrac (box dp) 1
  newP    <- getWavePointList gen stdUniform vec
  let newArr = (setPoint dp) // [(ix, newP)]
  return (newP, dp { setPoint = newArr })













-- ^^^^^^^^^^^^^^^ GARBAGE
testProperTetrahedron::SetPoint -> [PointPointer] -> Simplex -> Bool
testProperTetrahedron sP ps sigma = if isRadiusOK && isCenterOK && isSphereOK
                                    then True
                                    else error $ "--> Puta Merda " ++ show (isCenterOK, isSphereOK)
    where
    error_precisson = (10e-8)
    (pA,pB,pC,pD)  = setCellID sigma
    center         = circumSphereCenter sigma
    radius         = circumRadius sigma
    isRadiusOK     = error_precisson > (abs $ radius - (norm $ sP!pA - center))
    cleanP         = filter (\i -> (i /= pA) && (i /= pB) && (i /= pC) && (i /= pD)) ps
    -- | Test if the it is the center of the simplex
    isCenterOK     = and $ map testCenter [pA,pB,pC,pD]
    testCenter i   = error_precisson > (abs $ (norm $ center - sP!i) - radius)
    -- | Test if the CircumSphere is empty
    isSphereOK     = and $ map testEmptySph cleanP
    testEmptySph i = if radius < norm (sP!i - center)
                     then True
                     else error $ "--< Puta Merda " ++ show (i, sP!i, sigma)
