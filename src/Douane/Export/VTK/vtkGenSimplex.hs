module Douane.Export.VTK.VTKGenSimplex
       ( renderVTK
       , writeVTKfile
       ) where

import Control.Monad.State.Lazy
import Data.Array.Diff (listArray, (!), elems, bounds, (//), DiffArray)
import Data.IntMap (IntMap)
import Data.XML.Types
import qualified Data.IntMap as IM
import qualified Text.XML.Enumerator.Document as X

import Math.DeUni (Simplex (..), SetPoint, SetSimplex, PointPointer )
import Douane.Export.VTK.TemplateVTKxml


type Render = State VTKRender

data VTKRender = VTKRender
                 { cellConn     :: [Int]
                 , cellOffset   :: [Int]
                 , cellPointer  :: Int
                 , cellType     :: [Int] }
                 deriving (Show)
                       
initState = VTKRender
                 { cellConn     = []
                 , cellOffset   = []
                 , cellPointer  = 0
                 , cellType     = [] }
                 
writeVTKfile::FilePath -> SetPoint -> SetSimplex -> IO ()
writeVTKfile name arr gs = X.writeFile name (renderVTK arr gs)

renderVTK::SetPoint -> SetSimplex -> Document
renderVTK arr x = renderVTKDoc np nc dataPoint dataCell p c
  where
    ss = IM.toList x
    st = execState (mapM_ (renderSimplex.snd) ss) (initState)
    (lb,hb)        = bounds arr
    ps             = [lb..hb]
    np = length ps
    nc = IM.size x
    dataPoint = (renderScalarPointData "Pscalar" $ map fromIntegral [0..hb]):[]
    dataCell = (renderScalarCellData "ID" $ map fromIntegral $ map fst ss):[]
    
    p = renderPoints (map (\i->if i >= lb &&
                                  i <= hb
                               then Just $ arr!i
                               else Nothing ) [0..hb])
    
    c = renderCells (cellConn st) (cellOffset st) (cellType st) [] []

renderSimplex::Simplex -> Render ()
renderSimplex sigma = do
  st <- get
  let 
    (a,b,c,d) = setCellID sigma
    ps = map fromIntegral [a,b,c,d]
  modify (\x -> x { cellConn     = ps ++ (cellConn x) })
  modify (\x -> x { cellOffset   = (cellOffset x) ++ [ (cellPointer x) + 4 ] })
  modify (\x -> x { cellPointer  = cellPointer x + 4 })
  modify (\x -> x { cellType     = 10:(cellType x) })






