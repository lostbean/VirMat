{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module VirMat.IO.Import.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Control.Applicative ((<$>))
import Control.Monad (mzero, mplus)

-- Data definition
data JobRequest =
  VoronoiJob
    { dimension    ::Dimension
    , distrType    ::DistributionType
    , targetNumber ::Int
    , gsDist       ::[CombDist]
    , seed         ::RandomSeed
    , outputFile   ::OutputFile
    , showResults  ::ShowResults
    } deriving (Show)

data Dimension =
    Dimension2D
  | Dimension3D
  deriving (Show, Eq)

data DistributionType =
    RandomDistribution
  | PackedDistribution
  deriving (Show, Eq)

data OutputFile =
    NoOutput
  | OutputFile
    { outputFilePath ::FilePath
    , outputInfo     ::[OutputInfoType]
    } deriving (Show)

data OutputInfoType =
    GrainVolume
  | GrainArea
  | GrainNumber
  deriving (Show, Eq)

data ShowResults =
    ShowAll [ShowType]
  | NoShow
  deriving (Show)

data ShowType =
    ShowVoronoiGrains
  | ShowBox
  | ShowHull
  | ShowPoints
  | ShowSimplex
  deriving (Show, Eq)

data RandomSeed =
    Seed Int
  | NoSeed
  deriving (Show)


data CombDist = forall a . Distribution a =>  CombDist a

instance Show CombDist where
  show (CombDist x) = show x

getDistArea (CombDist x) = distArea x
getDistMean (CombDist x) = distMean x
getDistMode (CombDist x) = distMode x
getDistFunc (CombDist x) = distFunc x
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

-- ====================== JSON FrontEnd ========================
$(deriveToJSON defaultOptions ''CombDist)
-- $(deriveFromJSON defaultOptions ''CombDist)
instance FromJSON CombDist where
  parseJSON a@(Object v) = let
    ln = CombDist <$> (parseJSON a :: Parser LogNormal)
    n  = CombDist <$> (parseJSON a :: Parser Normal)
    u  = CombDist <$> (parseJSON a :: Parser Uniform)
    in (ln `mplus` n `mplus` u)
  parseJSON _          = mzero

$(deriveToJSON   defaultOptions ''LogNormal)
$(deriveFromJSON defaultOptions ''LogNormal)

$(deriveToJSON   defaultOptions ''Normal)
$(deriveFromJSON defaultOptions ''Normal)

$(deriveToJSON   defaultOptions ''Uniform)
$(deriveFromJSON defaultOptions ''Uniform)

$(deriveToJSON   defaultOptions ''RandomSeed)
$(deriveFromJSON defaultOptions ''RandomSeed)

$(deriveToJSON   defaultOptions ''ShowType)
$(deriveFromJSON defaultOptions ''ShowType)

$(deriveToJSON   defaultOptions ''ShowResults)
$(deriveFromJSON defaultOptions ''ShowResults)

$(deriveToJSON   defaultOptions ''OutputInfoType)
$(deriveFromJSON defaultOptions ''OutputInfoType)

$(deriveToJSON   defaultOptions ''OutputFile)
$(deriveFromJSON defaultOptions ''OutputFile)

$(deriveToJSON   defaultOptions ''DistributionType)
$(deriveFromJSON defaultOptions ''DistributionType)

$(deriveToJSON   defaultOptions ''Dimension)
$(deriveFromJSON defaultOptions ''Dimension)

$(deriveToJSON   defaultOptions ''JobRequest)
$(deriveFromJSON defaultOptions ''JobRequest)
