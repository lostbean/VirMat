
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Module definition
module ShowData where

-- external imports
import Graphics.Rendering.OpenGL (GLfloat, Vertex3, Color4, PrimitiveMode)
import Graphics.UI.GLUT hiding (Face, Color)
import Data.Vec hiding (map,head)
import Data.Array ((!))

-- internal imports
import DelaunayReverseOnion (Face, Simplex, SetSimplexPoint, Box(xMax,yMax,zMax,xMin,yMin,zMin),
                faceRef, setSimplexPointer)
import VoronoiCreator

-- data definition
data Structure3D = DT [Tetrahedron3D] | VN [Grain3D] | Box Box3D | Base Base3D

data Tetrahedron3D = Tetrahedron3D {
                                    tetraColor::Color4 GLfloat,
                                    tetra3DPoints::(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
                                   }
data Grain3D = Grain3D {
                        grainColor::Color4 GLfloat,
                        grain3DPoints::[[Vertex3 GLfloat]]
                       }

data Box3D = Box3D {
                    colorBox::Color4 GLfloat,
                    xBoxRange::(GLfloat, GLfloat),
                    yBoxRange::(GLfloat, GLfloat),
                    zBoxRange::(GLfloat, GLfloat)
                   }

data Base3D = Base3D (Color4 GLfloat) (Vertex3 GLfloat)


-- Show Instances
instance Show Vec3D where
    show f = "|" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ "|"
        where (Vec3D a b c) = f

-- instance Show Face where
--    show f = "|" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ "|"
--        where (a, b, c) = faceRef f

instance Show VoronoiGrain where
    show (VoronoiGrain n faces) = "Grain " ++ (show n) ++ " :" ++ concatMap showFaces faces ++ "\n"
        where
            showFaces (VoronoiFace (x:xs) _ _ (a,b)) = "\n\tface " ++ show a ++ " -> " ++ show b ++ concatMap (\x -> "\n\t\t|- " ++ showEdge x) xs ++ "\n\t\t|_ " ++ showEdge x
            showEdge (VoronoiEdge (a,b)) = "edge " ++ (showVertex a) ++ " <-> " ++ (showVertex b)
            showVertex (VoronoiVertex x _) = show x

-- |Create a class of type that are able to draw 3D things in OpenGL
class Show3DStructure a where
    show3D::a -> IO()

instance Show3DStructure [Tetrahedron3D] where
    show3D a = do
        mapM_  show3D a

instance Show3DStructure Tetrahedron3D where
    show3D tetra = do
        let (pA, pB, pC, pD) = tetra3DPoints tetra
        currentColor $= tetraColor tetra
        renderPrimitive Triangles $ do
            vertex pA
            vertex pB
            vertex pC

            vertex pA
            vertex pB
            vertex pD

            vertex pB
            vertex pC
            vertex pD

            vertex pC
            vertex pA
            vertex pD

        currentColor $= Color4 1 1 1 0.5
        renderPrimitive LineStrip $ do
            vertex pA
            vertex pB

            vertex pB
            vertex pC

            vertex pC
            vertex pA

            vertex pA
            vertex pD

            vertex pB
            vertex pD

            vertex pC
            vertex pD


instance Show3DStructure Grain3D where
    show3D grain = do
        currentColor $= grainColor grain
        renderPrimitive Polygon $ do
            let makeFace fs = mapM_ vertex fs
            mapM_ makeFace (grain3DPoints grain)
        currentColor $= Color4 1 1 1 1
        renderPrimitive LineLoop $ do
            let makeFace fs = mapM_ vertex fs
            mapM_ makeFace (grain3DPoints grain)

instance Show3DStructure [Grain3D] where
    show3D a = do
        mapM_  show3D a

instance Show3DStructure Base3D where
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


instance Show3DStructure Box3D where
    show3D a = do
        currentColor $= colorBox a
        let (xmax, xmin) = xBoxRange a
            (ymax, ymin) = yBoxRange a
            (zmax, zmin) = zBoxRange a
            upRigFro = vertex $ Vertex3 xmax ymax zmax
            upLefFro = vertex $ Vertex3 xmax ymin zmax
            upRigBac = vertex $ Vertex3 xmax ymax zmin
            upLefBac = vertex $ Vertex3 xmax ymin zmin
            botRigFro = vertex $ Vertex3 xmax ymax zmax
            botLefFro = vertex $ Vertex3 xmax ymin zmax
            botRigBac = vertex $ Vertex3 xmax ymax zmin
            botLefBac = vertex $ Vertex3 xmax ymin zmin
        renderPrimitive LineStrip $ do
            upRigFro; upLefFro; upRigBac; upLefBac; upRigFro

        renderPrimitive LineStrip $ do
            botRigFro; botLefFro; botRigBac; botLefBac; botRigFro

        renderPrimitive Lines $ do
            upRigFro; botRigFro
            upLefFro; botLefFro
            upRigBac; botRigBac
            upLefBac; botLefBac



make3DData::(SetSimplexPoint, [Simplex]) -> [Tetrahedron3D]
make3DData (setPoint, simplexs) = map (getColorPonit.setSimplexPointer) simplexs
    where
        getColorPonit (a,b,c,d) = Tetrahedron3D (calcColorPos a b c d) (makePoint $ setPoint!a, makePoint $ setPoint!b, makePoint $ setPoint!c, makePoint $ setPoint!d)
        calcColorPos a b c d = Color4 0.0 0.1 ((fromIntegral c)/10) 0.6
        makePoint (Vec3D p1 p2 p3) = Vertex3 (realToFrac p1) (realToFrac p2) ((realToFrac p3))


make3DDataG::Box -> VertexPointArray -> [VoronoiGrain] -> [Grain3D]
make3DDataG box vertexPointArray = map (make3DDataG' box vertexPointArray)

make3DDataG'::Box -> VertexPointArray -> VoronoiGrain -> Grain3D
make3DDataG' box vertexPointArray (VoronoiGrain n faces) = Grain3D getColor (map showFaces faces)
        where

            getColor = Color4 (realToFrac $ a/(xMax box)) (realToFrac $ b/(yMax box)) (realToFrac $ c/(zMax box)) 1
                -- TODO bug with head fnuc
                where (Vec3D a b c) = case faces of
                                        [] -> error $ "Grian with no face. Empty faces in grain " ++ show n
                                        [x] -> error $ "Grian with no face. Empty faces in grain " ++ show n
                                        (x:xs) -> centroid x
            showFaces (VoronoiFace (x:xs) _ _ _) = (showEdge x) (map showOneEdge xs)
            showOneEdge (VoronoiEdge (a,b)) = showVertex a
            showEdge (VoronoiEdge (a,b)) xs = (showVertex a):(showVertex b):xs
            showVertex (VoronoiVertex x _) = makePoint (vertexPointArray!x)
            makePoint (Vec3D p1 p2 p3) = Vertex3 (realToFrac p1) (realToFrac p2) ((realToFrac p3))

make3DBox::Box -> Box3D
make3DBox box = Box3D { colorBox = Color4 1 1 1 1,
                        xBoxRange = ((realToFrac.xMin) box, (realToFrac.xMax) box),
                        yBoxRange = ((realToFrac.yMin) box, (realToFrac.yMax) box),
                        zBoxRange = ((realToFrac.zMin) box, (realToFrac.zMax) box)}
