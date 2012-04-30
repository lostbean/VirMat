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
    { distrType    ::DistributionType
    , targetNumber ::Int
    , gsDist       ::[CombDist]
    , seed         ::RandomSeed
    , outputFile   ::OutputFile 
    , showResults  ::ShowResults
    } deriving (Show)

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

getDistFunc  (CombDist x) = distFunc x
getDistMean  (CombDist x) = distMean x
getDistScale (CombDist x) = distScale x


class (Show a, ToJSON a, FromJSON a) => Distribution a where
  distScale :: a -> Double
  distMean  :: a -> Double
  distFunc  :: a -> (Double -> Double)


instance Distribution LogNormal where
  distScale = logNormalScale
  distMean dist = logNormalOffSet dist + logNormalMean dist
  distFunc (LogNormal{..}) x = let
    mu     = log logNormalMode + sigma2
    sigma2 = (2/3) * log (logNormalMean / logNormalMode) 
    b      = (x * sigma2 * sqrt( 2 * pi))
    a      = exp (-1 * c)
    c      = ((log x - mu) ** 2) / (2 * sigma2 ** 2)
    in if x > 0 then a/b else 0
  
instance Distribution Normal where
  distScale = normalScale
  distMean  = normalMean
  distFunc (Normal{..}) x = let
    b = (normalVar * sqrt (2 * pi))
    a = exp (-1 * c)
    c = ((x - normalMean) ** 2) / (2 * normalVar ** 2)
    in normalScale * (a / b)
       
instance Distribution Uniform where
  distScale = uniformScale
  distMean  = uniformMean
  distFunc (Uniform{..}) x
    | -b <= a && a <= b = uniformScale / (2 * b)
    | otherwise         = 0
    where
      a = x - uniformMean
      b = uniformVar * sqrt 3

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
$(deriveToJSON id ''CombDist)
-- $(deriveFromJSON id ''CombDist)
instance FromJSON CombDist where
  parseJSON a@(Object v) = let
    ln = CombDist <$> (parseJSON a :: Parser LogNormal)
    n  = CombDist <$> (parseJSON a :: Parser Normal)
    u  = CombDist <$> (parseJSON a :: Parser Uniform)
    in (ln `mplus` n `mplus` u)
  parseJSON _          = mzero

$(deriveToJSON id ''LogNormal)
$(deriveFromJSON id ''LogNormal)

$(deriveToJSON id ''Normal)
$(deriveFromJSON id ''Normal)

$(deriveToJSON id ''Uniform)
$(deriveFromJSON id ''Uniform)

$(deriveToJSON id ''RandomSeed)
$(deriveFromJSON id ''RandomSeed)

$(deriveToJSON id ''ShowType)
$(deriveFromJSON id ''ShowType)

$(deriveToJSON id ''ShowResults)
$(deriveFromJSON id ''ShowResults)

$(deriveToJSON id ''OutputInfoType)
$(deriveFromJSON id ''OutputInfoType)

$(deriveToJSON id ''OutputFile)
$(deriveFromJSON id ''OutputFile)

$(deriveToJSON id ''DistributionType)
$(deriveFromJSON id ''DistributionType)

$(deriveToJSON id ''JobRequest)
$(deriveFromJSON id ''JobRequest)