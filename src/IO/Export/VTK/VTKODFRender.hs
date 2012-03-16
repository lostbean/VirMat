module IO.Export.VTK.VTKODFRender
       ( renderVTK
       , writeVTKfile )
       
       where

import qualified Data.Vector as V
import Data.XML.Types
import qualified Text.XML.Enumerator.Document as X

import Distributions.Texture.DiscreteODF ( DiscODF, step, nPHI1, nPHI, nPHI2, sPHI1, sPHI, sPHI2, odf )
import IO.Export.VTK.TemplateVTKXMLStruc

                 
writeVTKfile::FilePath -> DiscODF -> IO ()
writeVTKfile name df = X.writeFile name (renderVTK df)

renderVTK::DiscODF -> Document
renderVTK df = renderVTKDoc range origin stepSize [dataPoint] [dataCell]
  where
    range     = (0, nPHI1 df - 1, 0, nPHI df - 1, 0, nPHI2 df - 1)
    origin    = (0, 0, 0) 
    stepSize  = (step df, step df, step df)
    size      = V.length $ odf df
    dataPoint = renderScalarPointData "Intensity" $ V.toList $ odf df
    dataCell  = renderScalarCellData "Intensity" $ V.toList $ odf df



