
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}


-- Module definition
module Douane.Export.Viewer.ShowData where

-- external imports
import Data.Array.Diff (DiffArray, (!))
import Data.List ((!!))
import Data.Vec hiding (map, head, scale, translate, zipWith, length)
import Graphics.Rendering.OpenGL (GLfloat, Vertex3, Color4, PrimitiveMode)
import Graphics.UI.GLUT hiding (Face, Color)

-- internal imports
import Math.DeUni
    ( Face
    , Simplex
    , PointPointer 
    , Box (xMax,yMax,zMax,xMin,yMin,zMin)
    , facePoints
    , setCellID
    , circumSphereCenter
    , refND
    )
import Core.VoronoiBuilder
--import DeHull (SimplexHullFace, facePoints, outterND)

type SetPoint = DiffArray PointPointer Vec3D

-- data definition
data Point3D = Point3D { point3D   ::Vector3 GLfloat
                       , pointColor::Color4 GLfloat
                       }

data SimplexFace3D = SimplexFace3D { face3DPoints::(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
                                   , normal2face ::(Vertex3 GLfloat, Vertex3 GLfloat)
                                   , faceColor   ::Color4 GLfloat
                                   } deriving (Show)

data Tetrahedron3D = Tetrahedron3D { tetraColor   ::Color4 GLfloat
                                   , tetra3DPoints::(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
                                   }

data Grain3D = Grain3D { grainColor   ::Color4 GLfloat
                       , grain3DPoints::[[Vertex3 GLfloat]]
                       }

data Box3D = Box3D { colorBox ::Color4 GLfloat
                   , xBoxRange::(GLfloat, GLfloat)
                   , yBoxRange::(GLfloat, GLfloat)
                   , zBoxRange::(GLfloat, GLfloat)
                   }

data Base3D = Base3D (Color4 GLfloat) (Vertex3 GLfloat)


-- Show Instances
instance Show Vec3D where
    show f = "|" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ "|"
        where (Vec3D a b c) = f
{-
instance Show VoronoiGrain where
    show g = "Grain " ++ show (grainID g) ++ " :" ++ concatMap showSimplexFaces (faces g) ++ "\n"
        where showSimplexFaces = ("\n\t\t|- " ++).show

instance Show VoronoiFace where
    show f = "face to grain " ++ show (faceTo f) ++ " -> "  -- ++ (show $ map (edges f))
-}

-- | Using GADTs and Existential type to homogenize Show3D components into a list
data Renderable where
   Renderable :: Show3D b => [b] -> Renderable

-- |Create a class of type that are able to draw 3D things in OpenGL
class Show3D a where
    show3D::a -> IO()

instance (Show3D a) => Show3D [a] where
    show3D a = do
        mapM_  show3D a


instance Show3D Renderable where
    show3D (Renderable x) = show3D x


-- TODO Change convesion to outside
instance Show3D Point3D where
    show3D v = do
        currentColor $= pointColor v
        translate $ point3D v
        let w = 0.25::GLfloat
        renderPrimitive Quads $ do
            vertex $ Vertex3 w w w
            vertex $ Vertex3 w w (-w)
            vertex $ Vertex3 w (-w) (-w)
            vertex $ Vertex3 w (-w) w
            vertex $ Vertex3 w w w
            vertex $ Vertex3 w w (-w)
            vertex $ Vertex3 (-w) w (-w)
            vertex $ Vertex3 (-w) w w
            vertex $ Vertex3 w w w
            vertex $ Vertex3 w (-w) w
            vertex $ Vertex3 (-w) (-w) w
            vertex $ Vertex3 (-w) w w
            vertex $ Vertex3 (-w) w w
            vertex $ Vertex3 (-w) w (-w)
            vertex $ Vertex3 (-w) (-w) (-w)
            vertex $ Vertex3 (-w) (-w) w
            vertex $ Vertex3 w (-w) w
            vertex $ Vertex3 w (-w) (-w)
            vertex $ Vertex3 (-w) (-w) (-w)
            vertex $ Vertex3 (-w) (-w) w
            vertex $ Vertex3 w w (-w)
            vertex $ Vertex3 w (-w) (-w)
            vertex $ Vertex3 (-w) (-w) (-w)
            vertex $ Vertex3 (-w) w (-w)
        let Vector3 a b c = point3D v
        translate $ Vector3 ((-1)*a) ((-1)*b) ((-1)*c)

instance Show3D Tetrahedron3D where
    show3D tetra = do
        let (pA, pB, pC, pD) = tetra3DPoints tetra
            renderFace a b c = do
                currentColor $= tetraColor tetra
                renderPrimitive Triangles $ do
                    vertex a
                    vertex b
                    vertex c
                currentColor $= Color4 1 1 1 0.5
                renderPrimitive LineStrip $ do
                    vertex a
                    vertex b
                    vertex c
        renderFace pA pB pC
        renderFace pA pB pD
        renderFace pB pC pD
        renderFace pC pA pD

instance Show3D SimplexFace3D where
    show3D face = do
        let (pA, pB, pC) = face3DPoints face
            (nA, nB) = normal2face face
        --currentColor $= faceColor face
        --renderPrimitive Triangles $ do
        --    vertex pA
        --    vertex pB
        --    vertex pC
        currentColor $= Color4 1 1 1 0.8
        renderPrimitive LineLoop $ do
            vertex pA
            vertex pB
            vertex pC
        currentColor $= Color4 1 0 0 0.8
        renderPrimitive Lines $ do
            vertex nA
            vertex nB

instance Show3D Grain3D where
    show3D grain = do
        let
            makeSimplexFace fs = renderPrimitive Polygon  $ mapM_ vertex fs
            makeEdge fs = renderPrimitive LineLoop $ mapM_ vertex fs

        currentColor $= grainColor grain
        mapM_ makeSimplexFace (grain3DPoints grain)

        currentColor $= Color4 1 1 1 1
        mapM_ makeEdge (grain3DPoints grain)


instance Show3D Base3D where
    show3D a = do
        renderPrimitive LineStrip $ do
            currentColor $= Color4 1 0 0 1
            vertex (Vertex3 0 0 (0::GLfloat))
            vertex (Vertex3 1 0 (0::GLfloat))

            currentColor $= Color4 0 1 0 1
            vertex (Vertex3 0 0 (0::GLfloat))
            vertex (Vertex3 0 1 (0::GLfloat))

            currentColor $= Color4 0 0 1 1
            vertex (Vertex3 0 0 (0::GLfloat))
            vertex (Vertex3 0 0 (1::GLfloat))


instance Show3D Box3D where
    show3D a = do
        currentColor $= colorBox a
        let (xmax, xmin) = xBoxRange a
            (ymax, ymin) = yBoxRange a
            (zmax, zmin) = zBoxRange a
            upRigFro = vertex $ Vertex3 xmax ymax zmax
            upLefFro = vertex $ Vertex3 xmax ymin zmax
            upRigBac = vertex $ Vertex3 xmin ymin zmax
            upLefBac = vertex $ Vertex3 xmin ymax zmax

            botRigFro = vertex $ Vertex3 xmax ymax zmin
            botLefFro = vertex $ Vertex3 xmax ymin zmin
            botRigBac = vertex $ Vertex3 xmin ymin zmin
            botLefBac = vertex $ Vertex3 xmin ymax zmin
        renderPrimitive LineLoop $ do
            upRigFro; upLefFro; upRigBac; upLefBac

        renderPrimitive LineLoop $ do
            botRigFro; botLefFro; botRigBac; botLefBac

        renderPrimitive Lines $ do
            upRigFro; botRigFro
            upLefFro; botLefFro
            upRigBac; botRigBac
            upLefBac; botLefBac



renderTetrahedron3D::SetPoint -> Box -> [Simplex] -> [Tetrahedron3D]
renderTetrahedron3D sP box simplexs = map (getColorPonit.setCellID) simplexs
    where
        getColorPonit (a,b,c,d) = Tetrahedron3D (calcColorPos $ sP!a) (corners (sP!a,sP!b,sP!c,sP!d))
        calcColorPos (Vec3D a b c) = Color4 (realToFrac $ a/(xMax box)) (realToFrac $ b/(yMax box)) (realToFrac $ c/(zMax box)) 0.8
        corners (a,b,c,d) = (point2vertex a, point2vertex b, point2vertex c, point2vertex d)


renderGrain3D::Box -> [VoronoiGrain] -> [Grain3D]
renderGrain3D box = map renderGrain
    where
    renderGrain x = Grain3D getColor (map showSimplexFaces fs)
            where
                fs = faces x
                getColor = Color4 (realToFrac $ a/(xMax box)) (realToFrac $ b/(yMax box)) (realToFrac $ c/(zMax box)) 0.6
                    -- TODO bug with head fnuc
                    where (Vec3D a b c) = case fs of
                                            [] -> error $ "Grain with no face. Empty faces in grain."
                                            (x:xs) -> case edges x of
                                                [] -> error $ "Grain with invalid face."
                                                (y:ys) -> (circumSphereCenter.snd) y
                showSimplexFaces = (map $ point2vertex.circumSphereCenter.snd).edges


renderPoint3D::Int -> [Vec3D] -> [Point3D]
renderPoint3D color ps
    | color == 1 = map (conv (Color4 0 0 1 0.5)) ps
    | color == 2 = map (conv (Color4 0 1 0 0.5)) ps
    | otherwise  = map (conv (Color4 1 0 0 0.5)) ps
    where conv c x = Point3D (point2vector x) c


renderBox3D::Box -> Box3D
renderBox3D box = Box3D { colorBox = Color4 1 1 1 1,
                        xBoxRange = ((realToFrac.xMin) box, (realToFrac.xMax) box),
                        yBoxRange = ((realToFrac.yMin) box, (realToFrac.yMax) box),
                        zBoxRange = ((realToFrac.zMin) box, (realToFrac.zMax) box)}

renderHullFaces::SetPoint -> [Face] -> [SimplexFace3D]
renderHullFaces sP xs = zipWith convert [1..]  xs
    where
    convert i x = SimplexFace3D (point2vertex a, point2vertex b, point2vertex c) normal colorface
        where
        (ia, ib ,ic) = facePoints x
        (a, b, c)    = (sP!ia, sP!ib, sP!ic)
        nd           = refND x
        centerFace   = (a + b + c) / 3
        normal       = (point2vertex $ centerFace, point2vertex $ centerFace + nd)
        colorface    = Color4 1 1 (realToFrac $ k*i) 0.6
        k            = 1/(fromIntegral $ length xs)

packRender::(Show3D a) => [a] -> Renderable
packRender = Renderable

point2vertex (Vec3D p1 p2 p3) = Vertex3 (realToFrac p1) (realToFrac p2) ((realToFrac p3))

point2vector (Vec3D p1 p2 p3) = Vector3 (realToFrac p1) (realToFrac p2) ((realToFrac p3))
