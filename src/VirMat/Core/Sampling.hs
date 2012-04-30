{-# LANGUAGE NamedFieldPuns #-}


module VirMat.Core.Sampling
( sampleN
, linearDiscretization
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
import System.IO
import System.Random.Mersenne.Pure64
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Vector as V

-- Internal modules
import VirMat.IO.Export.SVG.RenderSVG
import VirMat.Distributions.GrainSize.StatTools

-- TODO temp remove
import Diagrams.Prelude ((<>), scaleY)
import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x


data Discrete a = DiscValue { discX::Double
                            , discY::a }

instance (Show a)=> Show (Discrete a) where
  show (DiscValue x y) = "DiscValue " ++ show (x,y)

linearDiscretization::(Double, Double) -> (Double -> Double) -> Int -> Vector (Discrete Double)
linearDiscretization (init, final) func n = let
  delta = final - init
  step  = if n < 0 then delta else delta / (fromIntegral n)
  xs    = V.fromList [init, init + step .. final]
  in V.map (\x -> DiscValue x (func x)) xs
  

sampleN::Vector (Discrete Double) -> IORef PureMT -> Int -> IO [Double]
sampleN dist gen n = do
  let int= integrate dist
  rnd <- replicateM n $ sampleFrom gen stdUniform
  return $ mapMaybe (invertFx int) rnd

sampleOne::Vector (Discrete Double) -> Double -> Maybe Double
sampleOne dist rnd = let
  int = integrate dist
  in invertFx int rnd

invertFx::Vector (Discrete Double) -> Double -> Maybe Double
invertFx integral y = let
  size = V.length integral 
  finder ia ib
    | ay > y && by > y = Nothing
    | ay < y && by < y = Nothing
    | (abs $ ib - ia) <= 1 = Just $ interpolate (a, b) y
    | otherwise = if isJust h1 then h1 else h2
    where
      h1 = finder ia half
      h2 = finder half ib
      half = (ia + ib) `div` 2
      a = integral!ia
      b = integral!ib
      ay = discY a
      by = discY b
  in if size > 0 then finder 0 (size-1) else Nothing

interpolate::(Discrete Double, Discrete Double) -> Double -> Double
interpolate (a, b) y = let
  ya = discY a
  yb = discY b
  xa = discX a
  xb = discX b
  ratio = (y-ya)/(yb-ya)
  in xa + (xb-xa)*ratio

normalize::Vector Double -> Vector Double
normalize df = norm
  where
    norm  = V.map (/max) df
    max = V.maximum df


-- | Calculate the cummulative function P(x), where P(x) is the integral
-- of f(t) from (-) infinity to x e.g. the sum of of the area from 
-- (-) infinity to x. More info, (see)[http://www.zweigmedia.com/RealWorld/integral/numint.html]
integrate::Vector (Discrete Double) -> Vector (Discrete Double)
integrate df = let
  area i a = let
    b     = df!(i+1)
    delta = discX b - discX a
    -- The integral of f(t) from a to b is the vaule of the area at b
    in (discX b, delta * 0.5 * (discY a + discY b))
  -- TODO: it can more effecient, add case for null Vec
  areas  = (discX $ V.head df, 0) `V.cons` V.imap area (V.init df)
  stairs = V.scanl1 (\a b -> a +  b) . V.map snd $ areas 
  in V.zipWith (\ref x -> DiscValue (fst ref) x) areas (normalize stairs)





-- TODO temp remove
test (a,b) n func = let
  disc    = linearDiscretization (a, b) func n
  discInt = integrate disc
  tolist  = V.toList . V.map (\(DiscValue x y) -> (x,y))
  dia     = renderPlot $ tolist disc
  diaInt  = renderPlot $ tolist discInt
  in do
    gen  <- getRandomGen NoSeed
    dist <- sampleN disc gen 100000
    print dist
 --   let diaHist = renderHistogram 1 13 0.2 $ map (*10) $ freqHist 1 13 0.2 dist
 --   renderSVGFile "/home/edgar/Desktop/plotInt.svg" (sizeSpec (Just 500, Just 500)) (scaleY 2 $ dia <> diaInt)