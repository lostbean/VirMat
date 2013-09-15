{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Distributions.Texture.ODFSampling
       ( addMicroFlexTexture
       , showVTKIPF
       ) where

import qualified Data.HashMap.Strict as HM

import           Control.Applicative ((<$>))

import           Hammer.MicroGraph
import           Hammer.Texture.Bingham
import           Hammer.Texture.IPF
import           Hammer.Texture.Orientation
import           Hammer.Texture.Symmetry

import           VirMat.Core.FlexMicro
import           VirMat.IO.Export.VTK.FlexMicro

--import           Debug.Trace
--debug s x = trace (s ++ show x) x

-- | Add orientation for each grain in a 'FlexMicro' data.
addMicroFlexTexture :: Bingham -> FlexMicro () -> IO (FlexMicro Quaternion)
addMicroFlexTexture dist fm@FlexMicro{..} = let
  grainmap = microGrains flexGraph
  ngrains  = HM.size grainmap
  in do
     gs <- sampleBingham dist ngrains
     let x = zipWith (\(k, v) q -> (k, setPropValue v q)) (HM.toList grainmap) gs
     return $ fm { flexGraph = flexGraph { microGrains = HM.fromList x } }

-- | Function to render IPF colors in VTK file.
showVTKIPF :: Symm -> RefFrame -> RenderGrainProp Quaternion
showVTKIPF symm ref = let
  unColor (RGBColor rgb) = rgb
  foo = unColor . getRGBColor . snd . getIPFColor symm ref
  in RenderGrainProp ("IPF " ++ show ref, \gid fm -> foo <$> (getPropValue =<< getGrainProp gid (flexGraph fm)))
