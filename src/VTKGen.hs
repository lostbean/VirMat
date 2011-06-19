module VTKGen ( renderVTK
              , writeVTKfile ) where

import Control.Monad.State.Lazy
import Data.XML.Types
import Data.Vec (Vec3D)
import qualified Text.XML.Enumerator.Document as X

import VoronoiBuilder (VoronoiGrain(..), VoronoiFace(..))
import Math.DeUni (Simplex, circumSphereCenter)
import TemplateVTKxml


type Render = State VTKRender

data VTKRender = VTKRender
                 { points       :: [Vec3D]
                 , pointPointer :: Int
                 , cellConn     :: [Int]
                 , cellOffset   :: [Int]
                 , cellPointer  :: Int
                 , cellType     :: [Int]
                 , facesIx      :: [Int] 
                 , faceOffset   :: [Int]
                 , facePointer  :: Int }
                 deriving (Show)
                       
initState = VTKRender
                 { points       = []
                 , pointPointer = 0
                 , cellConn     = []
                 , cellOffset   = []
                 , cellPointer  = 1
                 , cellType     = []
                 , facesIx      = [] 
                 , faceOffset   = []
                 , facePointer  = 1 }
                 
writeVTKfile::FilePath -> [VoronoiGrain] -> IO ()
writeVTKfile name gs = X.writeFile name (renderVTK gs)

renderVTK::[VoronoiGrain] -> Document
renderVTK x = renderVTKDoc np nc [] dataCell p c
  where
    st = execState (mapM_ renderGrain x) initState
    np = pointPointer st
    nc = length $ cellOffset st
    --renderScalarPointData
    dataCell = (renderScalarCellData "scalar" [1..(fromIntegral nc)]):[]
    p = renderPoints (points st)
    c = renderCells (cellConn st) (cellOffset st) (cellType st) (facesIx st) (faceOffset st)


renderGrain::VoronoiGrain -> Render ()
renderGrain gs = do 
  renderAllFaces (faces gs)
  st <- get
  modify (\x -> x {cellType=42:(cellType st)})
  
renderAllFaces::[VoronoiFace] -> Render ()
renderAllFaces fs = do
  allFaces <- mapM renderFace (map edges fs)
  st <- get
  let nFaces    = length fs
      allFaces' = concat allFaces
      offsetF   = facePointer st  + length allFaces' + 1
  modify (\x -> x { faceOffset = (faceOffset x) ++ [(offsetF - 1)] })
  modify (\x -> x { facePointer = offsetF })
  modify (\x -> x { facesIx = (nFaces:allFaces') ++ (facesIx x) })
  modify (\x -> x { cellOffset = (cellOffset x) ++ [(cellPointer x - 1)] })

renderFace::[Simplex] -> Render [Int]
renderFace ss = do
  pointer <- liftM pointPointer get
  let psIx = [pointer..(pointer + length ss - 1)]
      ps   = map circumSphereCenter ss
      np   = length ss
  modify (\x -> x { pointPointer = pointer + length ss })
  modify (\x -> x { points = (points x) ++ ps })
  modify (\x -> x { cellPointer = length ps + (cellPointer x) })
  modify (\x -> x { cellConn = psIx ++ (cellConn x) })
  return (np:psIx)







