module VirMat.IO.Export.Types where

-- Data definition
data Coord2D =
  Coord2D
  { x :: Double
  , y :: Double
  } deriving (Show)

data Histogram =
  Histogram
  { binSize :: Double
  , bins    :: [Coord2D] }
  deriving (Show)
