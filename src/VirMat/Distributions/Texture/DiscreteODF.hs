{-# LANGUAGE NamedFieldPuns #-}

module VirMat.Distributions.Texture.DiscreteODF
( ODFDim  (..)
, DiscODF (..)
-- Function to work with MTMDiscODF
-- TODO review export DiscODF constructor and getLinPos'
, getODF
, getLinPos
, getLinPos'
, getPos
, distributeOrientations
, distributeWeightedOrientations
, posToEuler
, errorDiscODF
, Euler
)where


import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Monad (liftM)

-- >>>>>>>>>>>>>>>>>>>>>>>>>   Data SECTION   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
type DegAng = Double

data ODFDim = Phi1 | PHI | Phi2 deriving (Show)

data DiscODF = DiscODF 
               { step ::DegAng
               , nPHI1::Int
               , nPHI ::Int
               , nPHI2::Int
               , sPHI1::DegAng
               , sPHI ::DegAng
               , sPHI2::DegAng
               , odf  ::Vector Double }
             deriving (Show)

getODF::DiscODF -> (Int,Int,Int) -> Double
getODF df pos = let n = getLinPos df pos
                in (odf df)!n

getLinPos::DiscODF -> (Int,Int,Int) -> Int
getLinPos df (i,j,k) = k*(nPHI df)*(nPHI1 df)+j*(nPHI1 df)+i

getLinPos'::Int -> Int -> (Int, Int, Int) -> Int
getLinPos' nPHI1 nPHI (i,j,k) = k*nPHI*nPHI1+j*nPHI1+i

getPos::DiscODF -> Int -> (Int,Int,Int)
getPos df n = (i,j,k)
  where
    (k,rk) = quotRem n $ (nPHI1 df)*(nPHI df)
    (j, i) = quotRem rk (nPHI1 df)

-- >>>>>>>>>>>>>>>>>>>>>>>>>   Data SECTION   <<<<<<<<<<<<<<<<<<<<<<<<<<<<

type Euler = (Double,Double,Double)

distributeWeightedOrientations::Vector (Euler,Double) -> Double -> ((Double,Int),(Double,Int),(Double,Int))-> DiscODF
distributeWeightedOrientations os step ((sPHI1,nPHI1),(sPHI,nPHI),(sPHI2,nPHI2)) = df {odf=dist}
  where
    linSize = if nPHI1*nPHI*nPHI2 > 0
              then nPHI1*nPHI*nPHI2
              else error "Can,t create discrete ODF. Number of bins null or negative."
    (hSet,hi,low) = V.foldl addtobin (IM.empty,0,0) os
    addtobin (hSet,hi,low) (pos, w)
      | lpos < 0       = (hSet,hi,low + w)
      | lpos > linSize = (hSet,hi + w,low)
      | otherwise            = (IM.insertWith (+) lpos w hSet, hi, low)
        where lpos = linPos pos
    id x start  = ceiling $ (x - start) / step
    pos (p1,p,p2) = (id p1 sPHI1, id p sPHI, id p2 sPHI2)
    linPos      = (getLinPos df) . pos
    dist        = V.generate linSize getCount
    getCount id = case IM.lookup id hSet of { Just x -> x; Nothing -> 0 }
    df          = DiscODF { step, nPHI1, nPHI, nPHI2, sPHI1, sPHI, sPHI2, odf = V.empty }

distributeOrientations::[Euler] -> Double -> ((Double,Int),(Double,Int),(Double,Int))-> DiscODF
distributeOrientations os step ((sPHI1,nPHI1),(sPHI,nPHI),(sPHI2,nPHI2)) = df {odf=dist}
  where
    one = 1
    linSize = if nPHI1*nPHI*nPHI2 > 0
              then nPHI1*nPHI*nPHI2
              else error "Can,t create discrete ODF. Number of bins null or negative."
    (hSet,hi,low) = foldl addtobin (IM.empty,0,0) os
    addtobin (hSet,hi,low) x
      | (linPos x) < 0       = (hSet,hi,low + one)
      | (linPos x) > linSize = (hSet,hi + one,low)
      | otherwise            = (IM.insertWith (+) (linPos x) one hSet, hi, low)
    id x start  = ceiling $ (x - start) / step
    pos (p1,p,p2) = (id p1 sPHI1, id p sPHI, id p2 sPHI2)
    linPos      = (getLinPos df) . pos
    dist        = V.generate linSize getCount
    getCount id = case IM.lookup id hSet of { Just x -> x; Nothing -> 0 }
    df          = DiscODF { step, nPHI1, nPHI, nPHI2, sPHI1, sPHI, sPHI2, odf = V.empty }
    
        
posToEuler::DiscODF -> (Int,Int,Int) -> Euler
posToEuler df (phi1,phi,phi2) = 
  ( ang phi1 $ sPHI1 df
  , ang phi  $ sPHI  df
  , ang phi2 $ sPHI2 df )
  where
    ang x start = (fromIntegral x)*(step df) + start

errorDiscODF::DiscODF -> DiscODF -> Maybe Double
errorDiscODF a b = 
  if step a     == step b
     && nPHI1 a == nPHI1 b 
     && nPHI a  == nPHI b 
     && nPHI2 a == nPHI2 b
     && sPHI1 a == sPHI1 b 
     && sPHI a  == sPHI b 
     && sPHI2 a == sPHI2 b
     && V.length aVec == V.length bVec
  then
    Just err
  else
    Nothing
  where
    err = V.ifoldl' (\acc i x -> acc + (x-bVec!i)**2) 0 aVec
    bVec = odf b
    aVec = odf a

