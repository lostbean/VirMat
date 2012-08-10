{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


module VirMat.Distributions.GrainSize.StatTools
       ( RandomSeed(..)
       , getRandomGen
       , freqHist
       , autoHistWeb
       , autoHist
       , composeDist
       , MultiDist (..)
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

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

data MultiDist =
  MultiDist
  { mDistFunc     :: (Double -> Double)
  , mDistMean     :: Double
  , mDistInterval :: (Double, Double)
  , mDistModes    :: [Double]
  , mDistArea     :: Double
  }

getRandomGen::RandomSeed -> IO (IORef PureMT)
getRandomGen x = case x of
  NoSeed -> newPureMT >>= newIORef
  (Seed seed) ->  (return $ pureMT (fromIntegral seed)) >>= newIORef

composeDist::[CombDist] -> Maybe MultiDist
composeDist [] = Nothing
composeDist fs = let
  dist  = foldl' (\acc dist -> (\x -> acc x + (getDistFunc dist) x)) (\_ -> 0) fs
  modes = map getDistMode fs
  area  = foldl' (\acc dist -> getDistArea dist + acc) 0 fs  
  
  mean = let
    func (sa, ma) dist = let
      s = getDistArea dist
      in (s + sa, (s*(getDistMean dist)^2) + ma)
    (totalS, totalM) = foldl' func (0,0) fs
    in sqrt (totalM / totalS)

  interval = let
    xs = concatMap (toList . getDistInterval) fs
    toList (a,b) = [a,b]
    in (minimum xs, maximum xs)
  
  in return $ trace (show (mean, interval, modes, area)) $ MultiDist
  { mDistFunc     = dist
  , mDistMean     = mean
  , mDistInterval = interval
  , mDistModes    = modes
  , mDistArea     = area
  }

freqHist::Double -> Double -> Int -> [Double] -> Histogram
freqHist initial final nbin dist = let
  step          = (final - initial) / (fromIntegral nbin)
  (total, hist) = foldl' add (0, IM.empty) dist

  calcBin x     =
    if total == 0
    then Coord2D {x = fromIntegral x * step, y = 0}
    else Coord2D {x = fromIntegral x * step, y = (IM.findWithDefault 0 x hist) / total}

  add (total, acc) x = let
    id = floor $ (x - initial) / step
    in (total + 1, IM.insertWith (+) id 1 acc)

  in Histogram {binSize = step, bins = map calcBin [0 .. nbin - 1]}

autoHistWeb :: [Double] -> Histogram
autoHistWeb = autoHist' 4

autoHist :: [Double] -> Histogram
autoHist = autoHist' 1

autoHist' :: Int -> [Double] -> Histogram
autoHist' k dist = let
  final = maximum dist
  init  = minimum dist
  -- overload number of bins so it can be recombined at the web interface
  over  = if k < 1 then 1 else k
  n     = (over*) . ceiling . sqrt . fromIntegral . length $ dist
  in freqHist init final n dist

{--
fracHist = undefined
fracHist

  add (total, acc) x = let
    id = floor $ (x - initial) / step
    in (total + x, IM.insertWith (+) id x acc)
--}
