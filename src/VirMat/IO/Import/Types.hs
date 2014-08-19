{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module VirMat.IO.Import.Types where

import qualified Data.Vector as V

import Data.Vector         (Vector)
import Control.Applicative ((<$>))
import Control.Monad       (mzero, mplus)

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- Data definition
data JobRequest =
  VoronoiJob
    { dimension    :: Dimension
    , distrType    :: DistributionType
    , targetNumber :: Int
    , gsDist       :: [CombDist]
    , seed         :: RandomSeed
    , output       :: Output
    } deriving (Show)

data Dimension =
    Dimension2D
  | Dimension3D
  deriving (Show, Eq)

data DistributionType =
    RandomDistribution
  | PackedDistribution Int
  deriving (Show, Eq)

data Output =
  Output
  { outputDir  :: FilePath
  , sampleName :: String
  , outputWhat :: [ShowType]
  } deriving (Show)

data ShowType =
    ShowVoronoi
  | ShowBox
  | ShowHull
  | ShowPoints
  | ShowSimplex
  | ShowForces
  deriving (Show, Eq)

data RandomSeed =
    Seed Int
  | NoSeed
  deriving (Show)

-- =================================== 1D distributions ==================================

data LogNormal =
  LogNormal
  { logNormalScale  :: Double
  , logNormalMean   :: Double
  , logNormalMode   :: Double
  , logNormalOffSet :: Double
  } deriving (Show)

data Normal =
  Normal
  { normalScale :: Double
  , normalMean  :: Double
  , normalVar   :: Double
  } deriving (Show)

data Uniform =
  Uniform
  { uniformScale :: Double
  , uniformMean  :: Double
  , uniformVar   :: Double
  } deriving (Show)

data CustomDist =
  CustomDist
  { customDist      :: Vector Double
  , customBinSize   :: Vector Double
  , customBinCenter :: Vector Double
  } deriving (Show)

data CombDist = forall a . Distribution a =>  CombDist a

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

class (Show a, ToJSON a, FromJSON a) => Distribution a where
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

-- ================================= Output Functions ====================================

getOutputFilePath :: Output -> String -> String -> FilePath
getOutputFilePath Output{..} extra ext = path
  where path = outputDir ++ "/" ++ sampleName ++ "-" ++ extra ++ "." ++ ext

-- ================================== JSON FrontEnd ======================================

$(deriveToJSON defaultOptions ''CombDist)
-- $(deriveFromJSON defaultOptions ''CombDist)
instance FromJSON CombDist where
  parseJSON a@(Object _) = let
    ln = CombDist <$> (parseJSON a :: Parser LogNormal)
    n  = CombDist <$> (parseJSON a :: Parser Normal)
    u  = CombDist <$> (parseJSON a :: Parser Uniform)
    c  = CombDist <$> (parseJSON a :: Parser CustomDist)
    in (ln `mplus` n `mplus` u `mplus` c)
  parseJSON _          = mzero

$(deriveToJSON   defaultOptions ''LogNormal)
$(deriveFromJSON defaultOptions ''LogNormal)

$(deriveToJSON   defaultOptions ''Normal)
$(deriveFromJSON defaultOptions ''Normal)

$(deriveToJSON   defaultOptions ''Uniform)
$(deriveFromJSON defaultOptions ''Uniform)

$(deriveToJSON   defaultOptions ''CustomDist)
$(deriveFromJSON defaultOptions ''CustomDist)

$(deriveToJSON   defaultOptions ''RandomSeed)
$(deriveFromJSON defaultOptions ''RandomSeed)

$(deriveToJSON   defaultOptions ''ShowType)
$(deriveFromJSON defaultOptions ''ShowType)

$(deriveToJSON   defaultOptions ''DistributionType)
$(deriveFromJSON defaultOptions ''DistributionType)

$(deriveToJSON   defaultOptions ''Dimension)
$(deriveFromJSON defaultOptions ''Dimension)

$(deriveToJSON   defaultOptions ''Output)
$(deriveFromJSON defaultOptions ''Output)

$(deriveToJSON   defaultOptions ''JobRequest)
$(deriveFromJSON defaultOptions ''JobRequest)
