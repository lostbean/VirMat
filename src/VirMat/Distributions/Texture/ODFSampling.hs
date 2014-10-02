{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Distributions.Texture.ODFSampling
       ( addMicroFlexTexture
       , getIPFRGBColor
       ) where

import qualified Data.HashMap.Strict as HM

import           Data.Word (Word8)

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

-- | Get a function for convert orientation in RGB IPF colors.
getIPFRGBColor :: Symm -> RefFrame -> (Quaternion -> (Word8, Word8, Word8))
getIPFRGBColor symm ref = unColor . getRGBColor . snd . getIPFColor symm ref
  where unColor (RGBColor rgb) = rgb
