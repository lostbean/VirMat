{-# LANGUAGE  RecordWildCards #-}

module Main where

import System.Environment (getArgs)

import VirMat.IO.Import.CommandLineInput
import VirMat.Run2D

import VirMat.IO.Import.Types
import Data.Aeson
import Debug.Trace
--import Data.Text.Lazy.Encoding
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

main = do
    putStrLn ("VirMat v0.3 04/2012 [by Edgar Gomes]")
    putStrLn  "____________________________________"
    jobReq <- getArgs >>= (return.parseArgs)    
 
-- ==================================== 2D ============================================= 
    simul <- runVirMat2D jobReq
    print $ getTargetGrainSizeHist simul
    printMicro     "Final" simul


