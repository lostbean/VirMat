{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Distributions.Texture.ODFSampling
       ( addMicroFlexTexture
       , showVTKIPF
       ) where

import qualified Data.HashMap.Strict as HM

import           Control.Applicative ((<$>))

import           Texture.Bingham
import           Texture.IPF
import           Texture.Orientation
import           Texture.Symmetry

import           VirMat.Core.FlexMicro

--import           Debug.Trace
--debug s x = trace (s ++ show x) x

-- | Add orientation for each grain in a 'FlexMicro' data.
addMicroFlexTexture :: (FlexMicroBuilder v)=> Bingham -> FlexMicro v ()
                    -> IO (FlexMicro v Quaternion)
addMicroFlexTexture dist fm = let
  gids = flexGrainList fm
  ngrains = length gids
  in do
     gs <- sampleBingham dist ngrains
     let qm = HM.fromList $ zip gids gs
     return $ modifyGrainProps (\gid _ -> maybe zerorot id (HM.lookup gid qm)) fm

-- | Function to render IPF colors in VTK file.
showVTKIPF :: Symm -> RefFrame -> RenderGrainProp v Quaternion
showVTKIPF symm ref = let
  unColor (RGBColor rgb) = rgb
  foo = unColor . getRGBColor . snd . getIPFColor symm ref
  in RenderGrainProp ( "IPF " ++ show ref, \_ prop -> foo <$> prop)
