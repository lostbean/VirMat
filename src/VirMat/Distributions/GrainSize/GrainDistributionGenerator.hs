{-# LANGUAGE
    TypeSynonymInstances
  , TypeFamilies
  , FlexibleInstances
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , RecordWildCards
  #-}
module VirMat.Distributions.GrainSize.GrainDistributionGenerator
  ( genGrainDistributionByNumber
  , genGrainDistributionByBox
  , DistributedPoints(..)
  , Ratio
  , Extent
  , defRatio
  , calcBox
  , boxDim
  , boxExt
  , genPoint
  ) where


import Data.IORef
import Data.Random
import DeUni.DeWall
import Linear.Vect
import System.Random.Mersenne.Pure64
import Data.Vector (Vector)
import qualified Data.Vector as V

import VirMat.Core.Sampling

data DistributedPoints a =
  DistributedPoints
  { box      :: Box a
  , setPoint :: SetPoint a
  }

class GenRandom v where
  type Ratio  v   :: *
  data Extent v   :: *
  -- | Bounding box ratio
  defRatio        :: Ratio v
  -- | Calculate the bounding box size based on the area/volume
  calcBox         :: Extent v -> Ratio v -> Box v
  -- | Calculate the bounding box size based on the population of weighted points.
  calcBoxFromDist :: Vector Double -> Ratio v -> Box v
  -- | Get the diagonal of the box
  boxDim          :: Box v -> v Double
  -- | Get the area/volume of the box
  boxExt          :: Box v -> Extent v
  -- | Calculate the area/volume based on the diameter of an circle/sphere
  diaToExt        :: Double -> Extent v
  -- | Sample a single point
  genPoint        :: IORef PureMT -> RVar Double -> IO (v Double)

instance GenRandom Vec2 where
  type    Ratio  Vec2 = (Double, Double)
  newtype Extent Vec2 = Area Double deriving (Eq, Ord, Num, Fractional, Show)

  defRatio = (1, 1)

  calcBox (Area area) (xRatio, yRatio) = let
    refRatio = xRatio
    k1       = yRatio / refRatio
    a        = sqrt (area / k1)
    in Box2D {xMax2D = a, xMin2D = 0, yMax2D = a * k1, yMin2D = 0}

  calcBoxFromDist rs ratio = let
    totalArea = V.foldl' (\acc d -> acc + diaToExt d) 0 rs
    in calcBox (1.1 * totalArea) ratio

  boxDim Box2D{..} = let
    dx = (xMax2D - xMin2D)
    dy = (yMax2D - yMin2D)
    in Vec2 dx dy

  boxExt Box2D{..} = let
    dx = (xMax2D - xMin2D)
    dy = (yMax2D - yMin2D)
    in Area $ abs (dx * dy)

  diaToExt d = Area $ d * d * pi/4

  genPoint gen f = do
    -- To avoid repetition on the pseudo-random generator,
    -- use one external gen
    -- wrapped in an StateMonad. Or for internal gen use:
    -- "gen <- getRandomGen"
    a <- sampleFrom gen f
    b <- sampleFrom gen f
    return $ Vec2 a b

instance GenRandom Vec3 where
  type    Ratio  Vec3 = (Double, Double, Double)
  newtype Extent Vec3 = Vol Double deriving (Eq, Ord, Num, Fractional, Show)

  defRatio = (1, 1, 1)

  calcBox (Vol vol) (xRatio, yRatio, zRatio) = let
    refRatio    = xRatio
    k1          = yRatio / refRatio
    k2          = zRatio / refRatio
    a           = (vol / (k1 * k2))**(1/3)
    in Box3D { xMax3D = a,      xMin3D = 0
             , yMax3D = a * k1, yMin3D = 0
             , zMax3D = a * k2, zMin3D = 0 }

  calcBoxFromDist rs = let
    totalVolume = V.foldl' (\acc d -> acc + diaToExt d) 0 rs
    in calcBox (1.1 * totalVolume)

  boxDim Box3D{..} = let
    dx = (xMax3D - xMin3D)
    dy = (yMax3D - yMin3D)
    dz = (zMax3D - zMin3D)
    in Vec3 dx dy dz

  boxExt Box3D{..} = let
    dx = (xMax3D - xMin3D)
    dy = (yMax3D - yMin3D)
    dz = (zMax3D - zMin3D)
    in Vol $ abs (dx * dy * dz)

  diaToExt d  = Vol  $ d * d * d * pi / 6

  genPoint gen f = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    a <- sampleFrom gen f
    b <- sampleFrom gen f
    c <- sampleFrom gen f
    return $ Vec3 a b c

getRandomGen :: Maybe Int -> IO (IORef PureMT)
getRandomGen a = case a of
  Nothing -> newPureMT >>= newIORef
  Just s  -> newIORef $ pureMT (fromIntegral s)

-- | Generate a distribution of weighted points (spheres/circles) where radius = sqrt (weight)
-- that follows the an user defined distribution of diameters. The number of points is also
-- defined by the user. The bounding box is automatic calculated.
genGrainDistributionByNumber :: (GenRandom v, Pointwise (v Double))
                             => Ratio v -> Maybe Int -> [CombDist] -> Int -> IO (DistributedPoints v)
genGrainDistributionByNumber ratio seed dist n = case composeDist dist of
    Just mdist -> do
      gen <- getRandomGen seed
      ds  <- sampleN mdist gen n
      ps  <- V.replicateM n (genPoint gen stdUniform)
      let
        -- Calculate the box size based on the total volume of
        -- sampled weighted points.
        box     = calcBoxFromDist ds ratio
        delta   = boxDim box
        wpoints = V.zipWith (\d p -> WPoint (d*d/4) (delta &! p)) ds ps
      return $ DistributedPoints box wpoints
    Nothing -> error "[GrainDistributionGenerator] No target distrubution defined."

-- | Generate a distribution of weighted points (spheres/circles) where radius = sqrt (weight)
-- that follows the an user defined distribution of diameters. The bounding box is also
-- defined by the user. The number of points is automatic calculated.
genGrainDistributionByBox :: (GenRandom v
                             , Ord (Extent v)
                             , Num (Extent v)
                             , Pointwise (v Double)
                             ) => Maybe Int -> [CombDist] -> Box v -> IO (DistributedPoints v)
genGrainDistributionByBox seed dist box = case composeDist dist of
    Just mdist -> do
      gen <- getRandomGen seed
      let vol = boxExt box
      ds  <- sampleVol mdist gen diaToExt vol
      ps  <- V.replicateM (V.length ds) (genPoint gen stdUniform)
      let
        delta   = boxDim box
        wpoints = V.zipWith (\d p -> WPoint (d*d/4) (delta &! p)) ds ps
      return $ DistributedPoints box wpoints
    Nothing -> error "[GrainDistributionGenerator] No target distrubution defined."
