{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


module VirMat.Distributions.GrainSize.StatTools  
       ( RandomSeed(..)
       , getRandomGen
       , freqHist
       , fracHist
       , composeDist  
       )where

-- External modules
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import qualified Data.List as L
import Control.Monad (liftM)
import Data.IORef
import System.Random.Mersenne.Pure64

import Hammer.Math.Vector hiding (Vector)

-- Internal modules
import VirMat.IO.Import.Types
import VirMat.IO.Export.Types

                        
getRandomGen::RandomSeed -> IO (IORef PureMT)
getRandomGen x = case x of
  NoSeed -> newPureMT >>= newIORef
  (Seed seed) ->  (return $ pureMT (fromIntegral seed)) >>= newIORef
    
composeDist::[CombDist] -> ((Double -> Double), Double)
composeDist fs = let
  dist = foldl' (\acc dist -> (\x -> acc x + (getDistFunc dist) x)) (\_ -> 0) fs
  mean = let
    func (sa, ma) dist = let s = getDistScale dist in (s + sa, (getDistMean dist * s) + ma)
    (totalS, totalM) = foldl' func (0,0) fs
    in totalM / totalS
  in (dist, mean)

freqHist::Double -> Double -> Double -> [Double] -> Histogram
freqHist initial final step dist = let
  n             = floor $ (final - initial) / step
  (total, hist) = foldl' add (0, IM.empty) dist
  
  calcBin x     = 
    if total == 0
    then Coord2D {x = fromIntegral x * step, y = 0}
    else Coord2D {x = fromIntegral x * step, y = (IM.findWithDefault 0 x hist) / total}
  
  add (total, acc) x = let 
    id = floor $ (x - initial) / step
    in (total + 1, IM.insertWith (+) id 1 acc)
  
  in Histogram {binSize = step, bins = map calcBin [0 .. n - 1]}


fracHist = undefined
   {--                
fracHist

  add (total, acc) x = let 
    id = floor $ (x - initial) / step
    in (total + x, IM.insertWith (+) id x acc)
--}