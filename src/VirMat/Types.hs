{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE  RecordWildCards #-}

module VirMat.Types where

import qualified Data.IntMap as IM

import           DeUni.DeWall
import           VirMat.Core.VoronoiMicro

data Simulation a = 
    Simulation
  { box           :: Box a
  , pointSet      :: SetPoint a
  , triangulation :: IM.IntMap (S2 a)
  , grainSet      :: VoronoiMicro a
  }


