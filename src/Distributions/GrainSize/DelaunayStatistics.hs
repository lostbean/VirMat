{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distributions.GrainSize.DelaunayStatistics
( logNormal 
, normal 
, printToFile
, printTargetToFile
, calcStat
, grainSize
, anisoShape
, nvizinho
, DistributionFunc (..)
, makeDistFunc
, findGSList
) where

-- External modules
import Data.Vec (Vec3D, norm, normalize, dot)
import qualified Data.IntMap as IM
import Control.Monad (liftM)

-- Internal modules
import Core.VoronoiBuilder (findGrainsTree, Level1(..), Level2(..), Level3(..))
import Math.DeUni ( Box(..)
                  , Simplex (..)
                  , Face (..)
                  , Point
                  , PointPointer (..)
                  , SetPoint
                  , SetSimplex )

newtype BinFreq =
  BinFreq Double
  deriving (Num, Fractional, Floating, Ord, Eq, Show)

data UniformBin = 
  UniformBin
  { hiBin   :: BinFreq
  , lowBin  :: BinFreq
  , binList :: [(Double, BinFreq)] }
  deriving (Show) 
                  
data UniformHist = 
  UniformHist
  { startPos :: Double
  , size     :: Double
  , n_bin    :: Int
  , bins     :: UniformBin }
  deriving (Show)
                            
data DistributionFunc = 
  DistFunc
  { distFunc :: (Double -> Double)
  , upperLim :: Double
  , lowerLim :: Double }

printToFile::FilePath -> UniformHist -> IO()
printToFile file dist = writeFile file (lowtxt ++ bintxt ++ hitxt)
  where
  bintxt = concatMap (\(b, BinFreq x) -> show b ++ "\t" ++ show x ++ "\n") (binList.bins $ dist)
  hitxt  = show hiPos ++ "\t" ++ (showBF.hiBin.bins $ dist) ++ "\n"
  lowtxt = "0.0\t" ++ (showBF.lowBin.bins $ dist) ++ "\n"
  hiPos  = binPos (size dist) (startPos dist) (n_bin dist + 1)
  showBF = show.(\(BinFreq x) -> x)

printTargetToFile::FilePath -> DistributionFunc -> IO()
printTargetToFile name dF = printToFile name udist
  where
    func  = distFunc dF
    lb    = lowerLim dF
    hb    = upperLim dF
    udist = discretize start size nbin func
    start = lb
    size  = abs $ (hb - lb)/(fromIntegral nbin)
    nbin  = 100
    
findGSList::SetPoint -> SetSimplex -> [Double]
findGSList sP wall = map (grainSize.getDEdges) $ findGrainsTree sP wall 

calcStat::SetPoint -> SetSimplex -> ([Vec3D] -> Double) -> DistributionFunc -> Maybe (Double, UniformHist)
calcStat sP wall prop dF = 
  liftM (\(BinFreq x) -> x) err >>= \x -> return (x,a)
  where
    func = distFunc dF
    lb   = lowerLim dF
    hb   = upperLim dF
    err  = getErrorDist a b
    a    = distibrute start size nbin stat
    b    = discretize start size nbin func
    stat = map (prop.getDEdges) $ findGrainsTree sP wall
    -- TODO Better boundary conditions
    start = lb
    size  = abs $ (hb - lb)/(fromIntegral nbin)
    nbin  = 100
    
getDEdges::Level3 -> [Vec3D]
getDEdges (L3 p _ nei) = map (\(L2 pn _ face) -> p - pn) nei

logNormal::Double -> Double -> Double -> Double
logNormal mu sigma x = a/b 
  where
  b = (x*sigma*sqrt(2*pi))
  a = exp (-1*c)
  c = ((log x - mu)**2)/(2*sigma**2)

normal::Double -> Double -> Double -> Double
normal mu sigma x = a/b 
  where
  b = (sigma*sqrt(2*pi))
  a = exp (-1*c)
  c = ((x - mu)**2)/(2*sigma**2)

grainSize::[Vec3D] -> Double
grainSize ls = sum / (2*n)
  where
  sum = foldl (\acc x -> (norm x) + acc) 0 ls 
  n   = fromIntegral.length $ ls
  
anisoShape::Vec3D -> [Vec3D] -> Double
anisoShape dir ls = sum / n
  where
  sum  = foldl (\acc x -> (ndir `dot` x) + acc) 0 ls 
  ndir = normalize dir
  n    = fromIntegral.length $ ls
  
nvizinho::[Vec3D] -> Double
nvizinho ls = fromIntegral.length $ ls

getErrorDist::UniformHist -> UniformHist -> Maybe BinFreq
getErrorDist a b = if ( startPos a == startPos b && size a == size b && n_bin a == n_bin b )
  then Just $ deltaBins+h+l
  else Nothing                 
  where
    deltaBins = foldl (+) (BinFreq 0) $ 
                zipWith (\(_, x) (_, y) -> (x-y)**2) (binList.bins $ a) (binList.bins $ b)
    l = ((hiBin.bins) a - (hiBin.bins) b)**2
    h = ((lowBin.bins) a - (lowBin.bins) b)**2

makeDistFunc::(Double -> Double) -> (Double, Double) -> DistributionFunc
makeDistFunc func (a, b)
  | a > b     = DistFunc { distFunc = func, upperLim = a, lowerLim = b }
  | otherwise = DistFunc { distFunc = func, upperLim = b, lowerLim = a }
              
distibrute::Double -> Double -> Int -> [Double] -> UniformHist
distibrute start size nbin ds = UniformHist
  { startPos = start
  , size     = size
  , n_bin    = nbin
  , bins     = normalizeDist $ uniBin }
  where
    one = BinFreq 1
    (hSet,hi,low) = foldl addtobin (IM.empty,0,0) ds
    addtobin (hSet,hi,low) x
      | (id x) < 0    = (hSet,hi,low + one)
      | (id x) > nbin = (hSet,hi + one,low)
      | otherwise     = (IM.insertWith (+) (id x) one hSet, hi, low)
    id x        = ceiling $ (x - start) / size
    listDist    = map (\id -> (binPos size start id, getCount id)) [1..nbin]
    getCount id = case IM.lookup id hSet of { Just x -> x; Nothing -> 0 }
    uniBin      = UniformBin { hiBin=hi, lowBin=low, binList=listDist }
  
binPos::Double -> Double -> Int -> Double
binPos size startPos id = (fromIntegral id)*size + startPos + size/2

normalizeDist::UniformBin -> UniformBin
normalizeDist dist = let
  (hSet, hi, low) = (binList dist, hiBin dist, lowBin dist)
  sum             = foldl (\acc (_,x) -> acc + x) 0 hSet
  total           = sum+hi+low
  bins            =  map (\(id,c) -> (id, c/total)) (binList dist)
  in UniformBin { hiBin=hi/total, lowBin=low/total, binList=bins }
                        
discretize::Double -> Double -> Int -> (Double -> Double) -> UniformHist
discretize start size nbin fdist = UniformHist
  { startPos = start
  , size     = size
  , n_bin    = nbin
  , bins     = normalizeDist uniBin }
  where
    bins   = map (\id -> (pos id, BinFreq $ fdist $ pos id)) [1..nbin]                
    uniBin = UniformBin { hiBin=0, lowBin=0, binList=bins }
    pos id = binPos size start id
                        
                        
                        
             