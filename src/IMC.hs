
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module IMC
( imc
, getFirstRandomGrainDistribution
) where


-- %%%%%%%%%% External dependences %%%%%%%%%%%
import Control.Monad (replicateM, liftM, foldM)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.ST (runST)
import Data.Random
import Data.Random.RVar
import Data.Random.Source.StdGen
import Data.IORef
import Data.Vec hiding (map, take, zipWith, length, head)
import System.Random.Mersenne.Pure64
import Data.Array.Diff (listArray, (!), elems, bounds, (//), DiffArray)
import Control.Monad (liftM)
import Data.IntMap (IntMap, empty)

-- %%%%%%%%%% Internal dependences %%%%%%%%%%%
import Math.DeUni (Box(..), isInBox, runDeWall, Simplex, PointPointer (..))
import CommandLineInput (RandomSeed(..))
import DelaunayStatistics (calcStat, printToFile)


import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

type ArrPoints = DiffArray PointPointer Vec3D
data IMCPoints = IMCPoints { arr::ArrPoints 
                           , box::Box }

imc::IMCPoints -> IORef PureMT -> (Double -> Double) -> (Double, Double) -> Double -> IO (IntMap Simplex)
imc imcP gen func (lb, hb) refErr = do
  newArr <- tossTheDice imcP gen
  let
    (lbArr, hbArr) = bounds newArr
    ps = [lbArr..hbArr]
    (wall, _) = runDeWall (box imcP) newArr ps
  (err, stat) <- calcStat newArr wall func lb hb
  print $ "err: " ++ show refErr ++ " -> " ++ show err
  let name = take 15 (show err) ++ dropWhile (/= 'e')  (show err)
  case err of
    (Just x) -> ze
      where
        ze
          | refErr == (-1) = imc imcP gen func (lb, hb) x
          | x < 1e-3       = return wall
          | x > refErr     = imc imcP gen func (lb, hb) refErr
          | x <= refErr    = printToFile ("stat"++name++".txt") stat >> imc (imcP {arr=newArr}) gen func (lb, hb) x
    Nothing -> return empty


tossTheDice::IMCPoints -> IORef PureMT -> IO ArrPoints
tossTheDice imcP gen = do
  let (PointPointer lb, PointPointer hb) = bounds (arr imcP)
  ix       <- liftM PointPointer $ sampleFrom gen (uniform lb hb)
  let vec  = boxFrac (box imcP) 0.5
  delta    <- getWavePointList gen stdUniform vec
  let  
    oldP = (arr imcP)!ix
    newArr = (arr imcP) // [(ix, oldP + delta)]
  print $ "point@" ++ show ix ++ " " ++ show oldP ++ " -> "  ++ show (oldP+delta)
  print $ isInBox (box imcP) (oldP + delta)
  if isInBox (box imcP) (oldP + delta)
    then return newArr
    else return (arr imcP)
  
boxFrac::Box -> Double -> (Double, Double, Double)  
boxFrac box frac = (dx*frac, dy*frac, dz*frac)  
  where
    dx = (xMax box - xMin box)
    dy = (yMax box - yMin box)
    dz = (zMax box - zMin box)

-- convToArr (DistributedPoints box list) = return.(IMCPoints box).(newListArray (1,targetNGrains))

getFirstRandomGrainDistribution::IORef PureMT -> Int -> Double -> (Double, Double, Double) -> IO IMCPoints
getFirstRandomGrainDistribution gen targetNGrains avgVolume ratio = result
    where
        totalVolume = avgVolume*(fromIntegral targetNGrains)
        fullbox = calcBoxSize totalVolume ratio
        dx = (xMax fullbox - xMin fullbox)
        dy = (yMax fullbox - yMin fullbox)
        dz = (zMax fullbox - zMin fullbox)

        getPoint = getWavePointList gen stdUniform (dx, dy, dz)
        
        result = do
          listP <- replicateM targetNGrains getPoint
          let a = listArray (1::PointPointer, fromIntegral targetNGrains) listP
          return (IMCPoints a fullbox)
          
calcBoxSize::Double -> (Double, Double, Double) -> Box
calcBoxSize avgVolume (xRatio, yRatio, zRatio) = Box {xMax,xMin,yMax,yMin,zMax,zMin}
    where
        cubeBoxEdge = avgVolume ** (1/3)

        refRatio = xRatio
        --k0 = xRatio/refRatio = 1
        k1 = yRatio/refRatio
        k2 = zRatio/refRatio
        a = (avgVolume/(k1*k2))**(1/3)

        xMax = a
        yMax = a*k1
        zMax = a*k2
        xMin = 0
        yMin = 0
        zMin = 0

getWavePointList::IORef PureMT -> RVar Double -> (Double, Double, Double) -> IO Vec3D
getWavePointList gen f (dx, dy,dz) = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    a <- sampleFrom gen f
    b <- sampleFrom gen f
    c <- sampleFrom gen f
    return $ Vec3D (a*dx) (b*dy) (c*dz)


getRandomGen::RandomSeed -> IO (IORef PureMT)
getRandomGen x = case x of
    NoSeed -> newPureMT >>= newIORef
    (Seed seed) ->  (return $ pureMT (fromIntegral seed)) >>= newIORef
