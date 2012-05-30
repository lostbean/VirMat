{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE  RecordWildCards #-}

module VirMat.Types where

import VirMat.Core.VoronoiBuilder

import Data.Vector (Vector, (!))
import qualified Data.IntMap as IM
import qualified Data.Map as Map

import DeUni.DeWall
import DeUni.Types
import DeUni.Dim3.Base3D
import DeUni.Dim2.Base2D
import Hammer.Math.Vector hiding (Vector)
import qualified Hammer.Math.Vector as AlgLin



data Simulation a = 
    Simulation
  { box           :: Box a
  , pointSet      :: SetPoint a
  , triangulation :: IM.IntMap (S2 a)
  , grainSet      :: [Grain a]
  }


