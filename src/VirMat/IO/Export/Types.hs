{-# LANGUAGE TemplateHaskell #-}

module VirMat.IO.Export.Types where

import Data.Aeson
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
$(deriveToJSON id ''Coord2D)
$(deriveFromJSON id ''Coord2D)

$(deriveToJSON id ''Histogram)
$(deriveFromJSON id ''Histogram)