{-# LANGUAGE NamedFieldPuns #-}

module VirMat.Distributions.Texture.DiscreteODF
       ( DiscODF (..)
       ) where

import qualified Data.Vector as V
import qualified Data.IntMap as IM
  
import Data.IntMap   (IntMap)
import Data.Vector   (Vector, (!))
import Control.Monad (liftM)

import Hammer.Math.Algerbra

-- ============================== Types =====================================

data DiscODF =
  DiscODF 
  { step  :: Rad
  , nPHI1 :: Int
  , nPHI  :: Int
  , nPHI2 :: Int
  , sPHI1 :: Rad
  , sPHI  :: Rad
  , sPHI2 :: Rad
  , odf   :: Vector Double
  } deriving (Show)

getODF :: DiscODF -> (Int, Int, Int) -> Double
getODF df pos = let
  n = getLinPos df pos
  in (odf df)!n

getLinPos :: DiscODF -> (Int, Int, Int) -> Int
getLinPos df (i,j,k) = k*(nPHI df)*(nPHI1 df)+j*(nPHI1 df)+i

getLinPos' :: Int -> Int -> (Int, Int, Int) -> Int
getLinPos' nPHI1 nPHI (i,j,k) = k*nPHI*nPHI1+j*nPHI1+i

getPos :: DiscODF -> Int -> (Int, Int, Int)
getPos df n = (i,j,k)
  where
    (k,rk) = quotRem n $ (nPHI1 df)*(nPHI df)
    (j, i) = quotRem rk (nPHI1 df)


