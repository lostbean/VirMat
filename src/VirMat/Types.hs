{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module VirMat.Types where

import qualified Data.IntMap as IM

import DeUni.DeWall
import VirMat.Core.VoronoiMicro

data Simulation a
    = Simulation
    { box :: Box a
    , pointSet :: SetPoint a
    , triangulation :: IM.IntMap (S2 a)
    , grainSet :: VoronoiMicro a
    }
