{-# LANGUAGE TemplateHaskell #-}

module VirMat.IO.Export.Types where

import Data.Aeson.TH

-- Data definition
data Coord2D =
  Coord2D
  { x:: Double
  , y:: Double
  } deriving (Show)

data Histogram =
  Histogram
  { binSize  :: Double
  , bins     :: [Coord2D] }
  deriving (Show)

-- ====================== JSON FrontEnd ========================

$(deriveToJSON   defaultOptions ''Coord2D)
$(deriveFromJSON defaultOptions ''Coord2D)

$(deriveToJSON   defaultOptions ''Histogram)
$(deriveFromJSON defaultOptions ''Histogram)
