{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module VirMat.IO.Import.Types where

import VirMat.Core.Sampling

data JobRequest
  = VoronoiJob
  { dimension  :: Dimension
  , structSize :: StructureSize
  , distrType  :: DistributionType
  , gsDist     :: [CombDist]
  , seed       :: Maybe Int
  , output     :: Output
  } deriving (Show)

data Dimension
  = Dimension2D
  | Dimension3D
  deriving (Show, Eq)

data StructureSize
  = NGrains Int
  | SizeBox (Double, Double, Double)
  deriving (Show, Eq)

data DistributionType
  = RandomDistribution
  | PackedDistribution Int
  deriving (Show, Eq)

data Output
  = Output
  { outputDir  :: FilePath
  , sampleName :: String
  , outputWhat :: [ShowType]
  } deriving (Show)

data ShowType
  = ShowVoronoi
  | ShowBox
  | ShowHull
  | ShowPoints
  | ShowSimplex
  | ShowForces
  deriving (Show, Eq)

-- ================================= Output Functions ====================================

getOutputFilePath :: Output -> String -> String -> FilePath
getOutputFilePath Output{..} extra ext = path
  where path = outputDir ++ "/" ++ sampleName ++ "-" ++ extra ++ "." ++ ext
