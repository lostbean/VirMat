module VirMat.Distributions.GrainSize.StatTools
  ( RandomSeed(..)
  , getRandomGen
  , freqHist
  , autoHistWeb
  , autoHist
  , composeDist
  , MultiDist (..)
  ) where

import qualified Data.IntMap as IM

import           Data.List     (foldl')

import           Data.IORef
import           System.Random.Mersenne.Pure64

import           VirMat.IO.Import.Types
import           VirMat.IO.Export.Types

--import Debug.Trace
--debug :: Show a => String -> a -> a
--debug s x = trace (s ++ show x) x

data MultiDist =
  MultiDist
  { mDistFunc     :: Double -> Double
  , mDistMean     :: Double
  , mDistInterval :: (Double, Double)
  , mDistModes    :: [Double]
  , mDistArea     :: Double
  }

getRandomGen :: RandomSeed -> IO (IORef PureMT)
getRandomGen a = case a of
  NoSeed   -> newPureMT >>= newIORef
  (Seed s) -> (return $ pureMT (fromIntegral s)) >>= newIORef

composeDist :: [CombDist] -> Maybe MultiDist
composeDist [] = Nothing
composeDist fs = let
  dist  = foldl' (\acc d -> (\a -> acc a + (getDistFunc d) a)) (const 0) fs
  modes = map getDistMode fs
  area  = foldl' (\acc d -> getDistArea d + acc) 0 fs

  mean = let
    func (sa, ma) d = let
      s = getDistArea d
      in (s + sa, (s * (getDistMean d) ^ (2 :: Int)) + ma)
    (totalS, totalM) = foldl' func (0,0) fs
    in sqrt (totalM / totalS)

  interval = let
    xs = concatMap (toList . getDistInterval) fs
    toList (a,b) = [a,b]
    in (minimum xs, maximum xs)

  in return MultiDist
  { mDistFunc     = dist
  , mDistMean     = mean
  , mDistInterval = interval
  , mDistModes    = modes
  , mDistArea     = area
  }

freqHist :: Double -> Double -> Int -> [Double] -> Histogram
freqHist initial final nbin dist = let
  step          = (final - initial) / (fromIntegral nbin)
  (total, hist) = foldl' add (0, IM.empty) dist

  calcBin b =
    if total == 0
    then Coord2D {x = fromIntegral b * step, y = 0}
    else Coord2D {x = fromIntegral b * step, y = (IM.findWithDefault 0 b hist) / total}

  add (totalsum, acc) b = let
    idnum = floor $ (b - initial) / step
    in (totalsum + 1, IM.insertWith (+) idnum 1 acc)

  in Histogram {binSize = step, bins = map calcBin [0 .. nbin - 1]}

autoHistWeb :: [Double] -> Histogram
autoHistWeb = autoHist' 4

autoHist :: [Double] -> Histogram
autoHist = autoHist' 1

autoHist' :: Int -> [Double] -> Histogram
autoHist' k dist = let
  upper = maximum dist
  lower = minimum dist
  -- overload number of bins so it can be recombined at the web interface
  over  = if k < 1 then 1 else k
  l     = fromIntegral $ length dist :: Double
  n     = (over *) . ceiling . sqrt $ l
  in freqHist lower upper n dist
