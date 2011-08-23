module Douane.Export.VTK.VTKGenTetra
       ( renderVTK
       , writeVTKfile
       ) where

import Control.Monad.State.Lazy
import Data.IntMap (IntMap)
import Data.Vec (Vec3D)
import Data.XML.Types
import qualified Data.IntMap as IM
import qualified Text.XML.Enumerator.Document as X

import Core.VoronoiBuilder (VoronoiGrain(..), VoronoiFace(..))
import Douane.Export.VTK.TemplateVTKxml
import Math.DeUni (Simplex, circumSphereCenter)


type Render = State VTKRender

data VTKRender = VTKRender
                 { points       :: IntMap Vec3D
                 , pointPointer :: Int
                 , cellConn     :: [Int]
                 , cellOffset   :: [Int]
                 , cellPointer  :: Int
                 , cellType     :: [Int]
                 , cellID       :: [Int] 
                 , cellIDPointer:: Int
                 , ncells       :: Int
                 , grainPointer :: Int }
                 deriving (Show)
                       
initState n = VTKRender
                 { points       = IM.empty
                 , pointPointer = 0
                 , cellConn     = []
                 , cellOffset   = []
                 , cellPointer  = 0
                 , cellType     = []
                 , cellID       = [] 
                 , cellIDPointer= 0
                 , ncells       = n
                 , grainPointer = 0 }
                 
writeVTKfile::FilePath -> [VoronoiGrain] -> IO ()
writeVTKfile name gs = X.writeFile name (renderVTK gs)

renderVTK::[VoronoiGrain] -> Document
renderVTK x = renderVTKDoc np nc dataPoint dataCell p c
  where
    st = execState (mapM_ renderGrain x) (initState $ length x)
    np = max + 1
    nc = length $ cellOffset st
    dataPoint = (renderScalarPointData "Pscalar" [0..(fromIntegral np)]):[]
    dataCell = (renderScalarCellData "ID" $ map fromIntegral $ cellID st):[]
    
    max = fst $ IM.findMax (points st)
    ps = map (\i -> IM.lookup i (points st)) [0..max]
    p = renderPoints (ps)
    
    c = renderCells (cellConn st) (cellOffset st) (cellType st) [] []


renderGrain::VoronoiGrain -> Render ()
renderGrain g = do 
  renderAllFaces (grainCenter g) (faces g)
  st <- get
  modify (\x -> x {grainPointer = (grainPointer x) + 1})
  
renderAllFaces::Vec3D -> [VoronoiFace] -> Render ()
renderAllFaces ct fs = do
  allFaces <- mapM (renderFace ct) (map edges fs)
  st <- get
  modify (\x -> x { cellIDPointer = (cellIDPointer x) +1 })  


renderFace::Vec3D -> [(Int, Simplex)] -> Render ()
renderFace ct ss = do
  st <- get
  let 
    nc           = ncells st
    pointer      = pointPointer st
    np           = length ss  
    gPointer     = grainPointer st
    ps           = map (\(i,x) -> (i+nc, circumSphereCenter x)) ss
    ps'          = (gPointer, ct):ps
    psIx         = map fst ss
    tetra        = renderTetra nc gPointer psIx
    nTetra       = length tetra `div` 4
    offsetList x = take nTetra [(x+4),(x+8)..]
    ids          = (\i->take nTetra [i,i..]).cellIDPointer $ st  
    types        = take nTetra [10,10..]
  
  modify (\x -> x { pointPointer = pointer + length ss })
  modify (\x -> x { points       = (points x) `IM.union` (IM.fromList ps') })
  modify (\x -> x { cellConn     = tetra ++ (cellConn x) })
  modify (\x -> x { cellOffset   = (cellOffset x) ++ (offsetList.cellPointer) x })
  modify (\x -> x { cellPointer  = cellPointer x + 4*nTetra })
  modify (\x -> x { cellType     = (cellType x) ++ types})
  modify (\x -> x { cellID       = (cellID x) ++ ids })



renderTetra inc x4 list = case list of
  (x3:xs) -> func x3 xs
  _       -> []
  where
    func x3 (x1:x2:xs) = (x4):(x3+inc):(x2+inc):(x1+inc):(func x3 (x2:xs))
    func _ _ = []





