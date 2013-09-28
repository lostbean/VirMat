{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module VirMat.Run2D where

import           VirMat.Types
import           VirMat.Core.VoronoiMicro
import           VirMat.Core.Packer
import           VirMat.Distributions.GrainSize.StatTools
import           VirMat.Distributions.GrainSize.GrainDistributionGenerator
import           VirMat.Distributions.GrainSize.GrainQuery
import           VirMat.IO.Import.Types
import           VirMat.IO.Export.Types
import           VirMat.IO.Export.SVG.RenderSVG

import qualified Data.Vector as V
import qualified Data.IntMap as IM

import           Data.Monoid        ((<>))
import           Diagrams.Prelude   ((===), (|||))
import           Text.Blaze.Html    (Html)

import           Hammer.Math.Algebra
import           DeUni.DeWall

--import           Debug.Trace
--debug  ::  Show a => String -> a -> a
--debug s x = trace (s ++ show x) x

-- ============================== 2D =====================================

runVirMat2D :: JobRequest -> IO (Simulation Point2D)
runVirMat2D jobReq = do
    (DistributedPoints box2D ps) <- case distrType jobReq of
        RandomDistribution -> genFullRandomGrainDistribution (1, 1) jobReq
        PackedDistribution -> do
          (DistributedPoints box2D ps0) <- genFullRandomGrainDistribution (1, 1) jobReq
          let
            len0         = V.length ps0
            psID0        = [0..len0-1]
            (wall0, _)   = runDelaunay2D box2D ps0 psID0
            (psFinal, _) = runPacker 60 box2D ps0 wall0
          return $ DistributedPoints box2D psFinal
    -- DEBUG
    let Just mdist = composeDist . gsDist $ jobReq
    print $ mDistMean mdist
    print $ mDistInterval mdist
    print $ mDistModes mdist
    print $ mDistArea mdist
    --print ps0
    -- =====
    let
      ls        = V.length ps
      psID      = [0 .. ls - 1]
      (wall, _) = runDelaunay2D box2D ps psID
      grains    = mkVoronoiMicro2D (onlySimpleInBox2D box2D wall)

    return $ Simulation { box           = box2D
                        , pointSet      = ps
                        , triangulation = wall
                        , grainSet      = grains }

onlyDistInBox :: (PointND a)=> Box a -> SetPoint a -> SetPoint a
onlyDistInBox box sp = V.filter ((isInBox box).point) sp

onlySimpleInBox2D :: Box Point2D -> IM.IntMap (S2 Point2D) -> IM.IntMap (S2 Point2D)
onlySimpleInBox2D box ls = IM.filter ((isInBox box).circleCenter) ls

-- ================== render to HTML ========================

renderSizeX :: Int
renderSizeX = 500

renderSizeY :: Int
renderSizeY = 500

renderPointSet :: Simulation Point2D -> Html
renderPointSet Simulation{..} = let
  dia = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D pointSet
  in renderSVGHtml (getSizeSpec (Just renderSizeX, Just renderSizeY)) dia

renderPointSetWithForces :: Simulation Point2D -> Html
renderPointSetWithForces Simulation{..} = let
  forces = setForce triangulation pointSet

  dia = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D pointSet
        <> renderForces pointSet forces
  in renderSVGHtml (getSizeSpec (Just renderSizeX, Just renderSizeY)) dia

renderGrainSet :: Simulation Point2D -> Html
renderGrainSet Simulation{..} = let
  dia = closeUpOnBox box $
           renderBox2D box
        <> renderSetGrain2D grainSet
  in renderSVGHtml (getSizeSpec (Just renderSizeX, Just renderSizeY)) dia

renderTriangulation :: Simulation Point2D -> Html
renderTriangulation Simulation{..} = let
  dia = closeUpOnBox box $
           renderBox2D box
        <> renderSetGrain2D grainSet
        <> renderSetS2Triangle pointSet triangulation
  in renderSVGHtml (getSizeSpec (Just renderSizeX, Just renderSizeY)) dia

-- ================== render to JSON ========================

--getGrainSizeHist :: Simulation Point2D -> Histogram
--getGrainSizeHist Simulation{..} = autoHist (getFaceAreaFracHist grainSet)

getTargetGrainSizeHist :: Simulation a -> Histogram
getTargetGrainSizeHist Simulation{..} = autoHist (map ((* pi).weight) $ V.toList pointSet)

-- ================== debug ========================
{--
printEvo :: Show t => t -> Box Point2D -> V.Vector (WPoint Vec2) -> IM.IntMap (S2 Vec2) -> [VoronoiFace Point2D] -> IO ()
printEvo nid box sp wall grains = let
  name = let
    n = show nid
    l = length n
    zeros = take (3 - l) ['0', '0' ..]
    in zeros ++ n
  forces = setForce wall sp

  diaUP1 = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D sp
        <> renderForces sp forces

  diaUP2 = closeUpOnBox box $
           renderBox2D box
        <> renderSetGrain2D grains

  in renderSVGFile
     ("evoluton_" ++ name ++ ".svg")
     (getSizeSpec (Just renderSizeX, Just renderSizeY)) $
     (diaUP1 ||| diaUP2)
--}

printMicro :: String -> Simulation Vec2 -> IO ()
printMicro name Simulation{..} = let
  forces = setForce triangulation pointSet
  disp   = setDisp triangulation pointSet

  diaUP1 = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D pointSet
        <> renderDisp pointSet disp

  diaUP2 = closeUpOnBox box $
           renderBox2D box
        <> renderSetPoint2D pointSet
        <> renderSetS2Triangle pointSet triangulation
        <> renderForces pointSet forces

  diaUP3 = closeUpOnBox box $
           renderBox2D box
        <> renderSetGrain2D grainSet

  in renderSVGFile
     ("microstructure_" ++ name ++ ".svg")
     (getSizeSpec (Just renderSizeX, Just renderSizeY)) $
     diaUP1 === diaUP2 === diaUP3
