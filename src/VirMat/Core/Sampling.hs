{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Core.Sampling
       ( sampleN
       , integrateMDist
       ) where

import qualified Data.IntMap as IM
import qualified Data.List   as L
import qualified Data.Vector as V

import Control.Monad             (liftM, replicateM)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Vector               ((!),Vector, (//))

import Control.Monad.Trans.Maybe
import Data.IORef
import Data.Maybe
import Data.Random
import Data.Random.RVar
import Data.Random.Source.StdGen
import System.IO
import System.Random.Mersenne.Pure64

import VirMat.IO.Export.SVG.RenderSVG
import VirMat.Distributions.GrainSize.StatTools



data Discrete a =
  DiscValue
  { discX :: Double
  , discY :: a
  }

instance (Show a)=> Show (Discrete a) where
  show (DiscValue x y) = "DiscValue " ++ show (x,y)

data IntegrationBin =
  IB
  { xmin :: Double
  , pmin :: Double
  , xmax :: Double
  , pmax :: Double
  , area :: Double
  } deriving (Show)

-- | Creates an initial set of bins with area based on the  upper and lower
-- limits and the list of modes. That should guarantee no distribution will be skipped.
getAreaBins :: MultiDist -> Vector (IntegrationBin)
getAreaBins MultiDist{..} = let
  xs    = V.fromList $ L.sort mDistModes
  min   = DiscValue (fst mDistInterval) 0
  max   = DiscValue (snd mDistInterval) 0
  modes = V.map (\x -> DiscValue x (mDistFunc x)) xs
  df    = min `V.cons` modes `V.snoc` max

  area i a = let
    b     = df!(i+1)
    delta = discX b - discX a
    in IB { xmin = discX a
          , pmin = discY a
          , xmax = discX b
          , pmax = discY b
          , area = delta * 0.5 * (discY a + discY b)
          }

  in V.imap area (V.init df)

integral_error :: Double
integral_error = 10e-5

-- | Divide and Conquer algorithm for subdivide in small bins until
-- their area error reaches a predefined minimum.
divConAreaBin :: (Double -> Double) -> IntegrationBin -> (Vector (IntegrationBin) -> Vector (IntegrationBin))
divConAreaBin func ia@IB{..} = let
  x     = (xmax + xmin) / 2
  p     = func x
  delta = x - xmin
  area1 = delta * 0.5 * (pmin + p)
  area2 = delta * 0.5 * (p + pmax)
  ia1   = ia { xmax = x, pmax = p, area = area1 }
  ia2   = ia { xmin = x, pmin = p, area = area2 }
  in if abs (area1 + area2 - area) > integral_error
    then divConAreaBin func ia1 . divConAreaBin func ia2
    else V.cons ia1 . V.cons ia2


-- | Calculate the cummulative function P(x), where P(x) is the integral
-- of f(t) from (-) infinity to x e.g. the sum of of the area from
-- (-) infinity to x. More info, (see)[http://www.zweigmedia.com/RealWorld/integral/numint.html]
integrateMDist :: MultiDist -> Vector (Discrete Double)
integrateMDist mdist = let
  func  = mDistFunc mdist
  bins  = getAreaBins mdist
  areas = V.foldr (\x acc -> divConAreaBin func x acc) V.empty bins
  init
    | V.null areas = error "[Sampling] Can't sampling undefined distribution."
    | otherwise    = (\x -> DiscValue (xmin x) 0) . V.head $ areas
  -- The integral of f(t) from a to b is the vaule of the area at b
  in V.scanl' (\acc x -> DiscValue (xmax x) (discY acc + area x)) init areas

-- | Find a vaule by inversion function. Given a Y vaule, find the corresponding
-- X value in the discrete cumulative function.
invertFx :: Vector (Discrete Double) -> Double -> Maybe Double
invertFx integral y = let
  size = V.length integral
  finder ia ib
    | ay > y && by > y = Nothing
    | ay < y && by < y = Nothing
    | (abs $ ib - ia) <= 1 = Just $ interpolate (a, b) y
    | otherwise = if isJust h1 then h1 else h2
    where
      h1 = finder ia half
      h2 = finder half ib
      half = (ia + ib) `div` 2
      a = integral!ia
      b = integral!ib
      ay = discY a
      by = discY b
  in if size > 0 then finder 0 (size-1) else Nothing

interpolate :: (Discrete Double, Discrete Double) -> Double -> Double
interpolate (a, b) y = let
  ya = discY a
  yb = discY b
  xa = discX a
  xb = discX b
  ratio = (y-ya)/(yb-ya)
  in xa + (xb-xa)*ratio

normalize :: Vector (Discrete Double) -> Vector (Discrete Double)
normalize df = let
  max = discY $ V.maximumBy (\a b -> compare (discY a) (discY b)) df
  in V.map (\x -> x {discY = (discY x) / max}) df

-- | Sample N diameter values from @MultiDist distribution.
sampleN :: MultiDist -> IORef PureMT -> Int -> IO (Vector Double)
sampleN dist gen n = do
  let int = normalize $ integrateMDist dist
  rnd <- V.replicateM n $ sampleFrom gen stdUniform
  return $ V.foldl' (\acc x -> case invertFx int x of
                        Just p -> p `V.cons` acc
                        _      -> acc
                    ) V.empty rnd

-- | Sample only one diameter value from @MultiDist distribution.
sampleOne :: Vector (Discrete Double) -> Double -> Maybe Double
sampleOne dist rnd = let
  int = normalize dist
  in invertFx int rnd
