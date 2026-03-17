module PackerTest where

import Data.Vector (Vector)
import qualified Data.Vector as V
import DeUni.DeWall
import Hammer.VTK
import Linear.Vect

import VirMat.Core.Packer (setForce)

-- | Test force computation for a series of distances
testForce :: [Double] -> [Double]
testForce = map (norm . force (WPoint 1 (Vec2 0 0)) . WPoint 5 . Vec2 0.1)

-- | Force between two weighted points
force :: (Norm Double v, PointND v) => WPoint v -> WPoint v -> v Double
force ref x
    | freeDist <= 0 =
        let
            f = 4 * freeDist
         in
            dir &* (-1 * f * f)
    | otherwise =
        let
            f = freeDist
         in
            dir &* f
  where
    delta = point x &- point ref
    dir = normalize delta
    ldelta = norm delta
    totalR = radius x + radius ref
    freeDist = ldelta - totalR

-- | Generate icosahedron vertices and triangles
icosahedron :: Double -> Vec3D -> (Vector Vec3D, Vector (Int, Int, Int))
icosahedron r pos =
    let
        t = (1 + sqrt 5) / 2 -- golden ratio
        k = 1 / sqrt (1 + t * t) -- correct the radius. For (0, 1, phi) the edge
        -- is 2 then the radius is sqrt(1+phi*phi)
        points =
            V.fromList
                [ (t, 1, 0)
                , (-t, 1, 0)
                , (t, -1, 0)
                , (-t, -1, 0)
                , (1, 0, t)
                , (1, 0, -t)
                , (-1, 0, t)
                , (-1, 0, -t)
                , (0, t, 1)
                , (0, -t, 1)
                , (0, t, -1)
                , (0, -t, -1)
                ]
        tri =
            V.fromList
                [ (0, 8, 4)
                , (0, 5, 10)
                , (2, 4, 9)
                , (2, 11, 5)
                , (1, 6, 8)
                , (1, 10, 7)
                , (3, 9, 6)
                , (3, 7, 11)
                , (0, 10, 8)
                , (1, 8, 10)
                , (2, 9, 11)
                , (3, 9, 11)
                , (4, 2, 0)
                , (5, 0, 2)
                , (6, 1, 3)
                , (7, 3, 1)
                , (8, 6, 4)
                , (9, 4, 6)
                , (10, 5, 7)
                , (11, 7, 5)
                ]
     in
        (V.map ((pos &+) . (r * k *&) . (\(x, y, z) -> Vec3 x y z)) points, tri)

-- | Write weighted points as VTK file using icosahedron approximation
writeWPointsVTKfile :: String -> SetPoint Vec3 -> IO ()
writeWPointsVTKfile file points =
    let
        vtks = V.imap foo points
        foo nid x =
            let
                (ps, cs) = icosahedron (radius x) (point x)
                psU = V.convert ps
                attr = mkCellAttr "GrainID" (\_ _ _ -> nid)
             in
                mkUGVTK "WPoint" psU cs [] [attr]
     in
        writeMultiVTKfile file True vtks
