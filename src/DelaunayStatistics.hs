-----------------------------------------------------------------------------
--
-- Module      :  VoronoiBuilder
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Edgar Gomes
-- Stability   :  dev
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DelaunayStatistics
( logNormal
, printToFile
, calcStat
, testDist
, testLogNorDist
) where

-- External modules
import Data.Vec (Vec3D, norm)
import qualified Data.IntMap as IM
import Control.Monad (liftM)

-- Internal modules
import VoronoiBuilder (findGrainsTree, Level1(..), Level2(..), Level3(..))

printToFile::FilePath -> UniformHist -> IO()
printToFile file dist = writeFile file (lowtxt ++ bintxt ++ hitxt)
  where
  bintxt = concatMap (\(b, BinFreq x) -> show b ++ "\t" ++ show x ++ "\n") (binList.bins $ dist)
  hitxt  = show hiPos ++ "\t" ++ (showBF.hiBin.bins $ dist) ++ "\n"
  lowtxt = "0.0\t" ++ (showBF.lowBin.bins $ dist) ++ "\n"
  hiPos  = binPos (size dist) (startPos dist) (n_bin dist + 1)
  showBF = show.(\(BinFreq x) -> x)



testDist gs = distibrute 0 0.2 200 $ map (avgHalfSize.getDEdges) gs
testLogNorDist = discretize 0 0.2 200 (logNormal 1 0.5)
calcErr gs = getErrorDist a b
  where
    a = testDist gs
    b = testLogNorDist

calcStat wall func lb hb = 
  return $ (liftM (\(BinFreq x) -> x) err, a)
  where
    err = getErrorDist a b
    a     = distibrute start size nbin stat
    b     = discretize start size nbin func
    stat  = map (avgHalfSize.getDEdges) $ findGrainsTree wall
    -- TODO Better boundary conditions
    start = lb
    size  = abs $ (hb - lb)/100
    nbin  = 100
    
logNormal::Double -> Double -> Double -> Double
logNormal mu sigma x = a/b 
  where
  b = (x*sigma*sqrt(2*pi))
  a = exp (-1*c)
  c = ((log x - mu)**2)/(2*sigma**2)

getDEdges::Level3 -> [Vec3D]
getDEdges (L3 p nei) = map (\(L2 pn face) -> p - pn) nei

avgHalfSize::[Vec3D] -> Double
avgHalfSize ls = sum/(fromIntegral $ 2*n)
  where
  sum = foldl (\acc x -> (norm x) + acc) 0 ls 
  n   = length ls
  
getErrorDist::UniformHist -> UniformHist -> Maybe BinFreq
getErrorDist a b = if ( startPos a == startPos b && size a == size b && n_bin a == n_bin b )
  then Just $ deltaBins+h+l
  else Nothing                 
  where
    deltaBins = foldl (+) (BinFreq 0) $ 
                zipWith (\(_, x) (_, y) -> (x-y)**2) (binList.bins $ a) (binList.bins $ b)
    l = ((hiBin.bins) a - (hiBin.bins) b)**2
    h = ((lowBin.bins) a - (lowBin.bins) b)**2

newtype BinFreq = BinFreq Double deriving (Num, Fractional, Floating, Ord, Eq, Show)

data UniformBin = UniformBin
                  { hiBin::BinFreq
                  , lowBin::BinFreq
                  , binList::[(Double, BinFreq)] }
                  deriving (Show) 
                  
data UniformHist = UniformHist
                   { startPos::Double
                   , size::Double
                   , n_bin::Int
                   , bins::UniformBin }
                   deriving (Show)
                            
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
                        
                        
                        
             