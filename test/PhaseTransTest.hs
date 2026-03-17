module PhaseTransTest where

import Hammer.MicroGraph (unGrainID)
import Hammer.VTK (writeMultiVTKfile)

import VirMat.Core.FlexMicro
import VirMat.Core.Sampling
import VirMat.IO.Import.Types
import VirMat.PhaseTrans

testJob :: JobRequest
testJob =
    VoronoiJob
        { dimension = Dimension2D
        , structSize = NGrains 500
        , distrType = RandomDistribution
        , gsDist = [CombDist distJob]
        , seed = Just 10
        , output = Output "" "" []
        }

distJob :: Normal
distJob =
    Normal
        { normalScale = 1
        , normalMean = 5
        , normalVar = 1
        }

runTest :: JobRequest -> IO ()
runTest job = do
    sim <- generateTransformation job
    let
        dir = "/Users/edgar/Desktop/"
        showTex = RenderGrainProp ("Value", \_ x -> fmap unGrainID x)
    writeMultiVTKfile (dir ++ "virmat-parent.vtu") True . renderFlexMicro [showGrainID, showTex] 1 . microParent $ sim
    writeMultiVTKfile (dir ++ "virmat-product2.vtu") True . renderFlexMicro [showGrainID, showTex] 1 . microProduct $ sim
