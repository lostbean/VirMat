{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module VirMat.Core.Sampling
       ( sampleN
       , sampleVol
       , sampleOne
       , integrateMDist
       -- * 1D distrubutions
       , LogNormal(..)
       , Normal(..)
       , Uniform(..)
       , CustomDist(..)
       , CombDist(..)
       , Distribution(..)
       , getDistArea
       , getDistMean
       , getDistMode
       , getDistFunc
       , getDistInterval
       , MultiDist(..)
       , composeDist
       ) where

import qualified Data.List   as L
import qualified Data.Vector as V

import Data.Vector   ((!), Vector)
import Data.Function (on)

import Data.IORef
import Data.Maybe
import Data.Random (sampleFrom, stdUniform)
import System.Random.Mersenne.Pure64

-- =================================== 1D distributions ==================================

data LogNormal
  = LogNormal
  { logNormalScale  :: Double
  , logNormalMean   :: Double
  , logNormalMode   :: Double
  , logNormalOffSet :: Double
  } deriving (Show)

data Normal
  = Normal
  { normalScale :: Double
  , normalMean  :: Double
  , normalVar   :: Double
  } deriving (Show)

data Uniform
  = Uniform
  { uniformScale :: Double
  , uniformMean  :: Double
  , uniformVar   :: Double
  } deriving (Show)

data CustomDist
  = CustomDist
  { customDist      :: Vector Double
  , customBinSize   :: Vector Double
  , customBinCenter :: Vector Double
  } deriving (Show)

data CombDist = forall a . (Show a, Distribution a )=> CombDist a

instance Show CombDist where
  show (CombDist x) = show x

getDistArea :: CombDist -> Double
getDistArea (CombDist x) = distArea x

getDistMean :: CombDist -> Double
getDistMean (CombDist x) = distMean x

getDistMode :: CombDist -> Double
getDistMode (CombDist x) = distMode x

getDistFunc :: CombDist -> Double -> Double
getDistFunc (CombDist x) = distFunc x

getDistInterval :: CombDist -> (Double, Double)
getDistInterval (CombDist x) = distInterval x

class Distribution a where
  distArea     :: a -> Double
  distMean     :: a -> Double
  distMode     :: a -> Double
  distFunc     :: a -> (Double -> Double)
  distInterval :: a -> (Double, Double)

instance Distribution LogNormal where
  distArea      = logNormalScale
  distMean dist = logNormalOffSet dist + logNormalMean dist
  distMode      = logNormalMode
  distFunc (LogNormal{..}) x = let
    mu     = log logNormalMode + sigma2
    sigma2 = (2/3) * log (logNormalMean / logNormalMode)
    b      = (x * sigma2 * sqrt( 2 * pi))
    a      = exp (-1 * c)
    c      = ((log x - mu) ** 2) / (2 * sigma2 ** 2)
    in if x > 0 then logNormalScale * (a / b) else 0
  distInterval (LogNormal{..}) = let
    mu     = log logNormalMode + sigma2
    sigma2 = (2/3) * log (logNormalMean / logNormalMode)
    sigma  = sqrt sigma2
    emu    = exp mu
    es     = exp sigma
    ess    = es * es * es
    in (emu * ess, emu / ess)

instance Distribution Normal where
  distArea = normalScale
  distMean = normalMean
  distMode = normalMean
  distFunc (Normal{..}) x = let
    sigma = sqrt normalVar
    b = (sigma * sqrt (2 * pi))
    a = exp (-1 * c)
    c = ((x - normalMean) ** 2) / (2 * normalVar)
    in normalScale * (a / b)
  distInterval (Normal{..}) = let
    threeS = 3 * (sqrt normalVar)
    in (normalMean - threeS, normalMean + threeS)

instance Distribution Uniform where
  distArea = uniformScale
  distMean = uniformMean
  distMode = uniformMean
  distFunc (Uniform{..}) x
    | -b <= a && a <= b = uniformScale / (2 * b)
    | otherwise         = 0
    where
      a = x - uniformMean
      b = sqrt (3 * uniformVar)

  distInterval (Uniform{..}) = let
    a = sqrt (3 * uniformVar)
    in (uniformMean - a, uniformMean + a)

instance Distribution CustomDist where
  distArea CustomDist{..} = V.sum $ V.zipWith (*) customDist customBinSize
  distMean CustomDist{..} = let
    a = V.sum $ V.zipWith (*) customDist customBinCenter
    b = V.sum customDist
    in a / b
  distMode CustomDist{..} = let
    i = V.maxIndex customDist
    in customBinCenter V.! i
  distFunc (CustomDist{..}) x = let
    r = V.findIndex (x <) customBinCenter
    func i
      | i < 1       = i
      | x >= middle = i
      | otherwise   = i - 1
      where middle = 0.5 * ((customBinCenter V.! i) + (customBinSize V.! (i-1)))
    in maybe 0 ((customDist V.!) . func) r
  distInterval (CustomDist{..}) = let
    ip = V.head customBinCenter
    is = V.head customBinSize
    fp = V.last customBinCenter
    fs = V.last customBinSize
    in (ip - is/2, fp + fs/2)

-- =================================== Merge distributions ===============================

data MultiDist
  = MultiDist
  { mDistFunc     :: Double -> Double
  , mDistMean     :: Double
  , mDistInterval :: (Double, Double)
  , mDistModes    :: [Double]
  , mDistArea     :: Double
  }

composeDist :: [CombDist] -> Maybe MultiDist
composeDist [] = Nothing
composeDist fs = let
  dist  = L.foldl' (\acc d -> (\a -> acc a + (getDistFunc d) a)) (const 0) fs
  modes = map getDistMode fs
  area  = L.foldl' (\acc d -> getDistArea d + acc) 0 fs

  mean = let
    func (sa, ma) d = let
      s = getDistArea d
      in (s + sa, (s * (getDistMean d) ^ (2 :: Int)) + ma)
    (totalS, totalM) = L.foldl' func (0,0) fs
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

-- ===================================== Invert Sampling =================================

data Discrete a =
  DiscValue
  { discX :: Double
  , discY :: a
  }

instance (Show a)=> Show (Discrete a) where
  show (DiscValue x y) = "DiscValue " ++ show (x, y)

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
getAreaBins :: MultiDist -> Vector IntegrationBin
getAreaBins MultiDist{..} = let
  xs    = V.fromList $ L.sort mDistModes
  lower = DiscValue (fst mDistInterval) 0
  upper = DiscValue (snd mDistInterval) 0
  modes = V.map (\x -> DiscValue x (mDistFunc x)) xs
  df    = lower `V.cons` modes `V.snoc` upper

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

integralerror :: Double
integralerror = 10e-5

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
  in if abs (area1 + area2 - area) > integralerror
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
  initDisc
    | V.null areas = error "[Sampling] Can't sampling undefined distribution."
    | otherwise    = (\x -> DiscValue (xmin x) 0) . V.head $ areas
  -- The integral of f(t) from a to b is the vaule of the area at b
  in V.scanl' (\acc x -> DiscValue (xmax x) (discY acc + area x)) initDisc areas

-- | Find a vaule by inversion function. Given a Y vaule, find the corresponding
-- X value in the discrete cumulative function.
invertFx :: Vector (Discrete Double) -> Double -> Maybe Double
invertFx integral y = let
  size = V.length integral
  finder ia ib
    | ay > y && by > y = Nothing
    | ay < y && by < y = Nothing
    | abs (ib - ia) <= 1 = Just $ interpolate (a, b) y
    | otherwise = if isJust h1 then h1 else h2
    where
      h1 = finder ia half
      h2 = finder half ib
      half = (ia + ib) `div` 2
      a = integral!ia
      b = integral!ib
      ay = discY a
      by = discY b
  in if size > 0 then finder 0 (size - 1) else Nothing

interpolate :: (Discrete Double, Discrete Double) -> Double -> Double
interpolate (a, b) y = let
  ya = discY a
  yb = discY b
  xa = discX a
  xb = discX b
  ratio = (y - ya) / (yb - ya)
  in xa + (xb - xa) * ratio

normalize :: Vector (Discrete Double) -> Vector (Discrete Double)
normalize df = let
  upper = discY $ V.maximumBy (compare `on` discY) df
  in V.map (\x -> x {discY = (discY x) / upper}) df

-- | Sample N diameter values from @MultiDist distribution.
sampleN :: MultiDist -> IORef PureMT -> Int -> IO (Vector Double)
sampleN dist gen n = do
  let int = normalize $ integrateMDist dist
  rnd <- V.replicateM n $ sampleFrom gen stdUniform
  return $ V.foldl' (\acc x -> case invertFx int x of
                        Just p -> p `V.cons` acc
                        _      -> acc
                    ) V.empty rnd

-- | Sample arbitrary number values from @MultiDist distribution until fulfill the volume/area.
-- INPUT: distributions, random generator, function to convert diameter to Vol/Area,
-- max. Vol/Arera. OUTPUT: list of diameters.
sampleVol :: (Ord b, Num b)=> MultiDist -> IORef PureMT -> (Double -> b) -> b -> IO (Vector Double)
sampleVol dist gen toVol maxVol = genUntilTotalM (nulvol, []) >>= return . V.fromList
  where
    nulvol = toVol 0
    int    = normalize (integrateMDist dist)

    --genUntilTotalM :: (b , [Double]) -> IO [Double]
    genUntilTotalM (acc, ds) = do
      rnd <- sampleFrom gen stdUniform
      let
        d = maybe 0 id (invertFx int rnd)
        v = toVol d
      if acc >= maxVol
        then return (d:ds)
        else genUntilTotalM (acc+v, d:ds)

-- | Sample only one diameter value from @MultiDist distribution.
sampleOne :: Vector (Discrete Double) -> Double -> Maybe Double
sampleOne dist rnd = let
  int = normalize dist
  in invertFx int rnd
