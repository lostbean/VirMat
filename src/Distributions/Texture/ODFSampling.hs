{-# LANGUAGE NamedFieldPuns #-}


module Distributions.Texture.ODFSampling
(testeIMCODF 
) where

-- External modules
import Control.Monad (liftM, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.IORef
import Data.Maybe
import Data.Random
import Data.Random.RVar
import Data.Random.Source.StdGen
import Data.Vector ((!),Vector, (//))
import IO
import System.Random.Mersenne.Pure64
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Vector as V

-- Internal modules
import Distributions.Texture.DiscreteODF
import Douane.Export.VTK.VTKODFWriter
import Douane.Import.MTF.MTMDiscreteODFReader

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x


pickUpOrietation::DiscODF -> IORef PureMT -> MaybeT IO (Euler)
pickUpOrietation odf gen = do
  let (lin, plan, vol) = integrate $ correctInvariance odf
  rnd1       <- lift $ sampleFrom gen stdUniform
  (n1, phi2) <- (MaybeT . return) $ invertFx vol rnd1
  rnd2       <- lift $ sampleFrom gen stdUniform
  (n2, phi)  <- (MaybeT . return) $ invertFx (plan!n1) rnd2
  rnd3       <- lift $ sampleFrom gen stdUniform
  (n3, phi1) <- (MaybeT . return) $ invertFx ((lin!n1)!n2) rnd3
  return $ (phi1,phi,phi2)


pickUpOrietation'::DiscODF -> Double -> Maybe Euler
pickUpOrietation' odf rnd = do
    let (lin, plan, vol) = integrate odf
    (n1, phi2) <- invertFx vol rnd
    (n2, phi)  <- invertFx (plan!n1) rnd
    (n3, phi1) <- invertFx ((lin!n1)!n2) rnd
    return (phi1,phi,phi2)

invertFx::Vector Double -> Double -> Maybe (Int, Double)
invertFx integral y = if size > 0
                      then finder 0 (size-1)
                      else Nothing
  where
    size = V.length integral 
    finder ia ib
      | a > y && b > y = Nothing
      | a < y && b < y = Nothing
      | (abs $ ib - ia) <= 1 = Just $ interpolate (ia, ib) 5.0 (a,b) y
      | otherwise = if isJust h1 then h1 else h2
        where
          h1 = finder ia half
          h2 = finder half ib
          half = (ia + ib) `div` 2
          a = integral!ia
          b = integral!ib
                              
interpolate::(Int, Int) -> Double -> (Double, Double) -> Double -> (Int, Double)
interpolate (ia,ib) step (ya,yb) y = (i,x)
  where
    xa = (fromIntegral ia + 0.5)*step
    xb = (fromIntegral ib + 0.5)*step
    x = xa + (xb-xa)*ratio
    ratio = (y-ya)/(yb-ya)
    i = if ratio >= 0.5 then ib else ia
  

--Correct the Euler space in inhomogeneity sin(PHI) dPhi1 dPHI dPhi2
correctInvariance::DiscODF -> DiscODF
correctInvariance df = df {odf = V.imap func (odf df)}
  where 
    func i x = let (_,iPHI,_) = getPos df i
               in  x*sin(delta*(fromIntegral iPHI))
    delta = (step df) * (pi/180)

normalize::Vector Double -> Vector Double
normalize df = norm
  where
    norm  = V.map (/max) df
    max = V.maximum df

integrate::DiscODF -> (Vector (Vector (Vector Double)) , Vector (Vector Double) , Vector Double)
integrate df = (normLines, normPlanes, normVol)
  where
    lines      = integrateLines df
    normLines  = V.map (V.map normalize) lines
    stepDeg    = step df
    planes     = integratePlanes stepDeg lines
    normPlanes = V.map normalize planes
    vol        = integrateVolume stepDeg planes
    normVol    = normalize vol
          
integrateLines::DiscODF -> Vector (Vector (Vector Double))
integrateLines df = newArr
  where
    delta   = (step df) * (pi/180)
    n1      = nPHI1 df - 1
    nP      = nPHI df  - 1
    n2      = nPHI2 df - 1
    funcF x = V.scanl1 (+) $ V.map int x
    int x   = delta*0.5*(getODF df x + getODF df (inteDir x))
    newArr  = V.map (V.map funcF) runSeq
    
    runSeq = V.fromList [V.fromList [V.fromList [(i,j,k) | i<-[0..n1-1]] | j<-[0..nP]] | k<-[0..n2]]
    inteDir (i,j,k) = (i+1, j, k)

integratePlanes::Double -> Vector (Vector (Vector Double)) -> Vector (Vector Double)
integratePlanes step df =  V.map (funcF . V.map V.maximum) df
  where
    delta = step * (pi/180)
    funcF = V.scanl1 (+) . int
    int vec = V.map (\x->delta*0.5*(vec!x + vec!(x+1))) (V.fromList [0..V.length vec - 2])


integrateVolume::Double -> Vector (Vector Double) -> Vector Double
integrateVolume step df = (funcF . V.map V.maximum) df 
  where
    delta = step * (pi/180)
    funcF = V.scanl1 (+) . int
    int vec = V.map (\x->delta*0.5*(vec!x + vec!(x+1))) (V.fromList [0..V.length vec - 2])









integratePVHODF::DiscODF -> DiscODF
integratePVHODF x = integratePVH Phi2 $ integratePVH PHI $ integratePVH Phi1 (correctInvariance x)

integratePVH::ODFDim -> DiscODF -> DiscODF
integratePVH dir df = newDF { odf = newArr }
  where
    delta = (step df) * (pi/180)
    n1 = nPHI1 df - 1
    nP = nPHI df  - 1
    n2 = nPHI2 df - 1
    funcF x = V.zip (V.map (getLinPos newDF) x) $ V.map int x
    int x = delta*0.5*(getODF df x + getODF df (inteDir x))
    newArr = (V.replicate n 0) `V.update` (V.concatMap funcF runSeq)
    n = nphi1' * nPHI' * nphi2'
    newDF = df { nPHI1 = nphi1', nPHI = nPHI', nPHI2 = nphi2', odf = V.empty }
    
    (nphi1', nPHI', nphi2') = case dir of
      Phi1 -> (n1, nP+1, n2+1)
      PHI  -> (n1+1, nP, n2+1)
      Phi2 -> (n1+1, nP+1, n2)
    
    runSeq = case dir of
      -- The list of comprihesion works in reverse mode e.g. [ ... | i<-[..], j<-[..]] will first change j and then i
      Phi1 -> V.fromList [V.fromList [(i,j,k) | i<-[0..n1-1]] | k<-[0..n2], j<-[0..nP]]
      PHI  -> V.fromList [V.fromList [(i,j,k) | j<-[0..nP-1]] | i<-[0..n1], k<-[0..n2]]
      Phi2 -> V.fromList [V.fromList [(i,j,k) | k<-[0..n2-1]] | j<-[0..nP], i<-[0..n1]]
    
    inteDir (i,j,k) = case dir of 
      Phi1 -> (i+1, j, k)
      PHI  -> (i, j+1, k)
      Phi2 -> (i, j, k+1)

normalizePVHODF::DiscODF -> DiscODF
normalizePVHODF df = df {odf = norm}
  where
    norm  = V.map (/total) $ odf df
    total = V.sum $ odf df
    
type Pos = (Int,Int,Int)

goUpStairs::DiscODF -> Int -> [Pos]
goUpStairs df n = ps
  where
    (_,ps,_) = V.ifoldl func (0, [], levels) (odf df)
    n' = fromIntegral n
    levels = map (\x -> (1/n')*(x-0.5)) [1..n']
    
    func (acc, cells, []) i x = (acc, cells, [])
    func (acc, cells, lev@(l:ls)) i x
      | l > acc   = (acc+x, cells, lev)
      | otherwise = func (acc, (getPos df i):cells, ls) i x
                    
                    
                    
                    
-- ########### Test Suit ###########
testePVH = do   
  df <- parseMTMDiscODF "../../ParaView/AODF.001" 
  let df'= normalizePVHODF $ integratePVHODF df
      dist = map (posToEuler df') $ goUpStairs df' 200000
      dist'= distributeOrientations dist 5 ((0,19),(0,19),(0,19))
  print $ length dist    
  writeVTKfile "/Users/edgar/Desktop/regenPVH.vti" dist'
                    
teste = do   
  df <- parseMTMDiscODF "../../ParaView/AODF.001"
  gen <- newPureMT >>= newIORef
  ls <- replicateM 50000 $ runMaybeT $ pickUpOrietation df gen
  let ls'= catMaybes ls
      dist'= distributeOrientations ls' 5 ((0,19),(0,19),(0,19))
  print $ length ls'    
  writeVTKfile "/Users/edgar/Desktop/regen.vti" dist'                    
  
testeIMCODF gs = do  
  df <- parseMTMDiscODF "AODF.001"
  gen <- newPureMT >>= newIORef
  imcODF gen df $ V.fromList gs
  
imcODF gen df gs = do   
  let n = V.length gs
      nGS = normalizeGS gs
  ls <- replicateM n $ runMaybeT $ pickUpOrietation df gen
  let ls'= V.fromList $ catMaybes ls
      fstDist = distributeWeightedOrientations fstTex 5 ((0,19),(0,19),(0,19))
      fstTex = V.zip ls' nGS
  print $ V.length ls'    
  writeVTKfile "ODFInit.vti" fstDist
  case errorDiscODF (normalizeODF df) (normalizeODF fstDist) of
    Just fstErr -> func fstTex fstErr
    _ -> error "REMOVE-MEs"
  where
    func tex refErr = do
      pointer <-  sampleFrom gen (uniform 0 (V.length tex - 1))
      newOri <- runMaybeT $ pickUpOrietation df gen
      case newOri of
        Just ori  -> case errorDiscODF (normalizeODF df) (normalizeODF dist) of
              Just err -> recursion err
              Nothing -> func tex refErr
          where
            tex' = V.update tex $ V.singleton (pointer, (ori, snd $ tex!pointer))
            dist = distributeWeightedOrientations tex' 5 ((0,19),(0,19),(0,19))
            name err = take 15 (show err) ++ dropWhile (/= 'e')  (show err)
            recursion err
              | err < 1e-4       = return dist
              | err > refErr     = func tex refErr
              | err <= refErr    = do
                print ("err: " ++ show refErr)
                writeVTKfile ("ODF_" ++ name err ++ ".vti") dist 
                func tex' err
        Nothing -> func tex refErr                    
    
    
    
    
    
normalizeGS::Vector Double -> Vector Double
normalizeGS df = norm 
  where
    norm  = V.map (/total) df
    total = V.sum df    
    
normalizeODF::DiscODF -> DiscODF
normalizeODF df = df {odf = norm}
  where
    norm  = V.map (/total) $ odf df
    total = V.sum $ odf df








printPHVOrig = do
  h <- hGetContents =<< openFile "../../ODFTay/out3" ReadMode
  let w = map words $ lines h
  ls <- mapM (mapM readIO) w :: IO [[Double]]
  let euler = map (\[a,b,c]->(a,b,c)) ls
  let dist = distributeOrientations euler 5.0 ((0,19),(0,19),(0,19))
  writeVTKfile "/Users/edgar/Desktop/PVH.vti" dist

    
sampleA = DiscODF { step=5, nPHI1 = 5, nPHI = 5, nPHI2 = 5, sPHI1 = 0, sPHI = 0, sPHI2 = 0, odf = V.generate (5*5*5) (\_ -> 1) }
                    