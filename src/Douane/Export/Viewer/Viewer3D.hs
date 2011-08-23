{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}


module Douane.Export.Viewer.Viewer3D (
showMap
)where

-- External modules
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Array
import Data.IORef
import Data.Vec --(Vec3D, Vec3I, Vec4I, dot, cross, norm, normSq)
import GHC.Float
import Graphics.Rendering.OpenGL hiding (normalize)
import Graphics.UI.GLUT hiding (normalize)
import System

-- Internal modules
import Douane.Export.Viewer.ShowData (show3D, Show3D, Tetrahedron3D, Renderable(..))
import Math.DeUni (Box(xMax,xMin,yMax,yMin,zMax,zMin))


import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

-- %%%%%%%%%%%%| Data definition |%%%%%%%%%%%%%%%%%%%%%%

-- | Define all types of control over the scene
data Control = MoveUp
             | MoveDown
             | MoveRight
             | MoveLeft
             | MoveForward
             | MoveBack
             | RotXc
             | RotXcw
             | RotYc
             | RotYcw
             | RotZc
             | RotZcw
             | ZoomIn
             | ZoomOut
             | ZoomAll
             | Idle
             deriving Show

-- | Store values about the scene, as camera, target and projection box (perspective or orthonormal)
data Scene = Scene { objPos         :: Vec3D
                   , cameraPos      :: Vec3D
                   , cameraUpDir    :: Vec3D
                   , isPerspective  :: Bool
                   , viewPortRate   :: Double
                   , lensAngleDeg   :: Double
                   , nearPlane      :: Double
                   , farPlane       :: Double
                   , targetBox      :: Box
                   , orthoLeft      :: Double
                   , orthoRigth     :: Double
                   , orthoTop       :: Double
                   , orthoBottom    :: Double
                   } deriving (Show)

type SetPoint = Array Int Vec3D

getWinSize::Int-> Int-> Size
getWinSize widthPoints heigthPoints
    | xratio < yratio = Size 800 (round $ 800*(xratio/yratio))
    | otherwise = Size (round $ 800*(yratio/xratio)) 800
    where
        xratio = (fromIntegral widthPoints)/800.0
        yratio = (fromIntegral heigthPoints)/800.0


display::IORef Control -> IORef Scene -> [Renderable] -> DisplayCallback
display contrlEnv scene vertexData = do
    clear [ColorBuffer, DepthBuffer]
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    blendEquation $= FuncAdd

    depthMask $= Enabled
    depthFunc $= Just Less

    loadIdentity
    control <- readIORef contrlEnv
    newscene <- (readIORef scene) >>= doMove <$> calcCamera control
    writeIORef scene newscene
    show3D vertexData
    flush


reshape :: IORef Control -> IORef Scene -> ReshapeCallback
reshape contrlEnv sceneEnv size@(Size w h) = do
    viewport $= (Position 0 0, size)
    loadIdentity
    control <- readIORef contrlEnv
    (readIORef sceneEnv) >>= doMove <$> calcCamera control
    return ()


mouse::MotionCallback
mouse (Position x y) = putStrLn $ "xy - " ++ show x ++ " : " ++ show y


keyboard::IORef Control -> KeyboardMouseCallback       -- where KeyboardMouseCallback means: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboard _ (Char '\27')           Down  _     _ = exitWith ExitSuccess
keyboard c (Char '+')             Down  _     _ = writeIORef c ZoomIn >> postRedisplay Nothing
keyboard c (Char '-')             Down  _     _ = writeIORef c ZoomOut >> postRedisplay Nothing
keyboard c (Char 'f')             Down  _     _ = writeIORef c MoveForward >> postRedisplay Nothing
keyboard c (Char 'b')             Down  _     _ = writeIORef c MoveBack >> postRedisplay Nothing
keyboard c (Char 'X')             Down  _     _ = writeIORef c RotXc >> postRedisplay Nothing
keyboard c (Char 'x')             Down  _     _ = writeIORef c RotXcw >> postRedisplay Nothing
keyboard c (Char 'Y')             Down  _     _ = writeIORef c RotYc >> postRedisplay Nothing
keyboard c (Char 'y')             Down  _     _ = writeIORef c RotYcw >> postRedisplay Nothing
keyboard c (Char 'Z')             Down  _     _ = writeIORef c RotZc >> postRedisplay Nothing
keyboard c (Char 'z')             Down  _     _ = writeIORef c RotZcw >> postRedisplay Nothing
keyboard c (SpecialKey KeyRight)  Down  _     _ = writeIORef c MoveRight >> postRedisplay Nothing
keyboard c (SpecialKey KeyLeft)   Down  _     _ = writeIORef c MoveLeft >> postRedisplay Nothing
keyboard c (SpecialKey KeyUp)     Down  _     _ = writeIORef c MoveUp >> postRedisplay Nothing
keyboard c (SpecialKey KeyDown)   Down  _     _ = writeIORef c MoveDown >> postRedisplay Nothing
keyboard _  _                      _    _    _ = return ()


calcProjection::Vec3D -> Scene -> Scene
calcProjection vecCamObj scene@(Scene { targetBox = box }) =
    let
        xBot    = xMin box
        xUp     = xMax box
        yBot    = yMin box
        yUp     = yMax box
        zBot    = zMin box
        zUp     = zMax box
        obj     = Vec3D ((xBot + xUp)/2) ((yBot + yUp)/2) ((zBot + zUp)/2)
        maxDiagBox      = Vec3D (abs (xBot - xUp)) (abs (yBot - yUp)) (abs (zBot - zUp))
        maxRadiusObj    = (norm maxDiagBox)/2.0
        camera  = (normalize vecCamObj) * (pack $ vec $ 3*maxRadiusObj) + obj
        near    = maxRadiusObj * 1.5
        far     = maxRadiusObj * 4.5
        left    = maxRadiusObj * 1.5
        right   = maxRadiusObj * 1.5
        top     = maxRadiusObj * 1.5
        bottom  = maxRadiusObj * 1.5
    in scene { objPos       = obj
             , cameraPos    = camera
             , nearPlane    = near
             , farPlane     = far
             , orthoLeft    = left
             , orthoRigth   = right
             , orthoTop     = top
             , orthoBottom  = bottom
             }


calcCamera::Control -> Scene -> Scene
calcCamera control scene = case control of
    MoveRight   ->  leftRigth (2)
    MoveLeft    ->  leftRigth (-2)
    MoveUp      ->  upDown (2)
    MoveDown    ->  upDown (-2)
    ZoomIn      ->  scene { lensAngleDeg = zoom 0.8 }
    ZoomOut     ->  scene { lensAngleDeg = zoom 1.25 }
    _           ->  scene
    where
        camera = cameraPos scene
        obj = objPos scene
        up = cameraUpDir scene
        vecCamObj = camera - obj
        vecUpObj = vecCamObj + up
        zoom rate = (lensAngleDeg scene) * rate

        -- Rotate Up/Down
        leftRigth stepSize = scene {cameraPos=newCamPos}
            where
                rotAxis = up
                newCamPos = obj + rot (deg2Rad stepSize) rotAxis vecCamObj

        -- Rotate Left/Rigth
        upDown stepSize = scene {cameraPos=newCamPos, cameraUpDir=newUp}
            where
                rotAxis = normalize $ pack $ (unpack up) `cross` (unpack vecCamObj)
                newCamPos = obj + rot (deg2Rad stepSize) rotAxis vecCamObj
                newUp = rot (deg2Rad stepSize) rotAxis up



doMove::Scene -> IO Scene
doMove scn@(Scene { objPos      = objPos
                  , cameraPos   = cameraPos
                  , cameraUpDir = cameraUpDir
                  , viewPortRate= viewRate
                  , lensAngleDeg= lens
                  , nearPlane   = near
                  , farPlane    = far
                  , orthoLeft   = left
                  , orthoRigth  = right
                  , orthoTop    = top
                  , orthoBottom = bottom
                  }) = do
    matrixMode $= Projection
    loadIdentity
    if isPerspective scn
        then perspective (realToFrac lens) (realToFrac viewRate) (realToFrac near) (realToFrac far)
        else ortho (realToFrac left) (realToFrac right) (realToFrac bottom) (realToFrac top) (realToFrac near) (realToFrac far)

    matrixMode $= Modelview 0
    loadIdentity
    lookAt (toVertex cameraPos) (toVertex objPos) (toVector cameraUpDir)

    return scn



-- TODO Move this funcs to a specilazeed module ---------------------------------------
-- | Convert a number in degrees to rads
deg2Rad::(Floating a) => a -> a
deg2Rad x = x*pi/180

-- |A formula which transforms a given coordinate system by rotating it through a
-- counterclockwise angle about an axis. The equation for the "fixed" vector in the
-- transformed coordinate system (Goldstein 1980; Varshalovich et al. 1988, p. 24)
rot::Double -> Vec3D -> Vec3D -> Vec3D
rot angle n r = a + b + c
    where
        cosine = cos angle
        a = r * pack (vec cosine)
        b = n * pack (vec ((n `dot` r) * (1 - cosine)))
        c = pack $ (unpack r `cross` unpack n) * vec (sin angle)
----------------------------------------------------------------------------------------


showMap::[Renderable] -> Box -> IO ()
showMap vertexData box = do
    let
        initScene = sceneZero box
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ SingleBuffered
                          , RGBAMode
                          , WithDepthBuffer
                          , WithAlphaComponent
                          ]
    initialWindowSize     $= getWinSize 200 300
    initialWindowPosition $= Position 100 100
    createWindow "Virmat"
    controlEnv  <- newIORef (Idle::Control)
    sceneEnv    <- newIORef (calcProjection (Vec3D 0 1 0)initScene)
    displayCallback       $= display controlEnv sceneEnv vertexData
    reshapeCallback       $= Just (reshape controlEnv sceneEnv)
    motionCallback        $= Just (mouse)
    keyboardMouseCallback $= Just (keyboard controlEnv)
    mainLoop

sceneZero::Box -> Scene
sceneZero box = Scene { objPos         = Vec3D 0 0 0
                      , cameraPos      = Vec3D 0 80 0
                      , cameraUpDir    = Vec3D 0 0 1
                      , isPerspective  = True
                      , viewPortRate   = 1
                      , lensAngleDeg   = 45
                      , nearPlane      = 0
                      , farPlane       = 0
                      , targetBox      = box
                      , orthoLeft      = 0
                      , orthoRigth     = 0
                      , orthoTop       = 0
                      , orthoBottom    = 0
                      }


toVector (Vec3D a b c) = Vector3 (realToFrac a) (realToFrac b) (realToFrac c)

toVertex (Vec3D a b c) = Vertex3 (realToFrac a) (realToFrac b) (realToFrac c)

