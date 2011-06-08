{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Delauny (
deWall,
Simplex(circumSphereCenter,setSimplexPointer),
SimplexPointer(..),
SetSimplexPoint,
Face,
faceRef,
Box(xMax,xMin,yMax,yMin,zMax,zMin,Box),
getBox,
isInBox,
onlySimpleInBox,
) where

import Prelude hiding (null)
import Data.Vec hiding (map, length, fromList, fold)
import Data.List (map, foldl', filter, head, (\\))
import Data.Array.IArray (Array, Ix, (!))
import Data.Set (Set, deleteFindMax, member, empty, null, delete, insert, fromList, fold)
import Data.Maybe
import Monad (liftM, liftM2)
import System.Random.Mersenne.Pure64

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x


type Point = Vec3D
type SetSimplexPoint = Array SimplexPointer Point
type SetSimplexFace = Set Face

newtype SimplexPointer = SimplexPointer Int deriving (Eq, Ord, Num, Ix, Real, Enum, Integral, Show)

-- Plane is represented by a vector from oring (0,0) to the closest point in the plane to the oring
-- TODO change plane to 4D vec to allow palen across the oring
data Simplex = Simplex {circumSphereCenter::Point, setSimplexPointer::(SimplexPointer, SimplexPointer, SimplexPointer, SimplexPointer) } deriving (Show)
data Plane = Plane {planeNormal::Vec3D, planeDist::Double} deriving (Show, Eq)
data Position = B1 | B2 | B1B2Plane | None deriving Show
data Face = Face { faceRef::(SimplexPointer, SimplexPointer, SimplexPointer), simplexAtNormal::Bool} deriving Show
data Box = Box { xMax::Double,
                 xMin::Double,
                 yMax::Double,
                 yMin::Double,
                 zMax::Double,
                 zMin::Double } deriving (Show)

instance Ord Face where
    compare a b = compare ((fast3DSort.faceRef) a) ((fast3DSort.faceRef) b)

instance Eq Face where
    x == y = compare x y == EQ

onlySimpleInBox::Box -> [Simplex] -> [Simplex]
onlySimpleInBox box ls = filter ((isInBox box).circumSphereCenter) ls


deWall::SetSimplexPoint -> [SimplexPointer] -> SetSimplexFace -> Box -> [Simplex]
deWall setPoint p afl box
    -- Recursive Triangulation
    | null afl1' && null afl2' = sigma''
    | (null) afl1' = deWall setPoint p2 afl2' box2 ++ sigma''
    | (null) afl2' = deWall setPoint p1 afl1' box1 ++ sigma''
    | otherwise  = deWall setPoint p1 afl1' box1 ++ deWall setPoint p2 afl2' box2 ++ sigma''
        where   (afl1', afl2', sigma'') = getSigma setPoint p (box1,box2) aflAlpha afl1 afl2 sigma'
                (plane, box1, box2) = debug " @@@ Box: " $ genPlane box
                (p1, p2) = pointSetPartition (box1, box2) setPoint p
                (afl', sigma') = fillAFL setPoint (afl, []) p1 p2 plane
                (aflAlpha, afl1, afl2) = fold (splitAF setPoint (box1,box2)) (empty,empty,empty) afl'


-- Simplex Wall Construction
getSigma::SetSimplexPoint -> [SimplexPointer] -> (Box,Box) -> SetSimplexFace -> SetSimplexFace -> SetSimplexFace -> [Simplex] -> (SetSimplexFace, SetSimplexFace, [Simplex])
getSigma setPoint p pairBox aflAlpha afl1 afl2 sigma
    | null aflAlpha = (afl1, afl2, sigma)
    | otherwise = case t of
        Just sig -> getSigma setPoint p pairBox aflAlpha' afl1' afl2' (sig:sigma)
        _ ->        getSigma setPoint p pairBox aflAlphaExtracted afl1 afl2 (sigma)
    where
        (f, aflAlphaExtracted) = deleteFindMax aflAlpha
        t = makeSimplex setPoint p f
        (Just sig) = t
        nexfs = extractFaces setPoint f sig
        (aflAlpha', afl1', afl2') = fold (splitAF setPoint pairBox) (aflAlphaExtracted, afl1, afl2) nexfs



fillAFL ::SetSimplexPoint -> (SetSimplexFace, [Simplex]) -> [SimplexPointer] -> [SimplexPointer] -> Plane -> (SetSimplexFace, [Simplex])
fillAFL setPoint (afl, sigma) p1 p2 plane
    | (not.null) afl = (afl, sigma)
    | otherwise = case t of
        Just sim -> (extractAllFaces setPoint sim , sim:sigma)
        _ -> (afl, sigma)
        -- TODO remove concat list
        where t = makeFirstSimplex setPoint plane p1 p2 (p1 ++ p2)


splitAF ::SetSimplexPoint -> (Box, Box) -> Face -> (SetSimplexFace,SetSimplexFace,SetSimplexFace) -> (SetSimplexFace,SetSimplexFace,SetSimplexFace)
splitAF setPoint pairBox f (aa, a1, a2) = case map (\i -> whereIsPoint pairBox (setPoint!i)) [a,b,c] of
    [B1,B1,B1] -> upP1
    [B2,B2,B2] -> upP2
    [B1,B1B2Plane,B1B2Plane] -> upP1
    [B2,B1B2Plane,B1B2Plane] -> upP2
    [B1B2Plane,B1,B1B2Plane] -> upP1
    [B1B2Plane,B2,B1B2Plane] -> upP2
    [B1B2Plane,B1B2Plane,B1] -> upP1
    [B1B2Plane,B1B2Plane,B2] -> upP2
    [None,_,_] -> (aa, a1, a2)
    [_,None,_] -> (aa, a1, a2)
    [_,_,None] -> (aa, a1, a2)
    _ -> upAlpha
    where   (a,b,c) = faceRef f
            upP1 =    (removeFace f aa, updateFace f a1, removeFace f a2)
            upP2 =    (removeFace f aa, removeFace f a1, updateFace f a2)
            upAlpha = (updateFace f aa, removeFace f a1, removeFace f a2)

removeFace::Face -> SetSimplexFace -> SetSimplexFace
removeFace (Face f side) set = delete ordFace set
    where ordFace = Face (fast3DSort f) side

-- Update list with active faces
updateFace::Face -> SetSimplexFace -> SetSimplexFace
updateFace (Face f side) set
    | member ordFace set = delete ordFace set
    | otherwise = insert ordFace set
    where ordFace = Face (fast3DSort f) side

-- get the others faces from given simplex (excluding the given face)
-- TODO find a more effecienty away, Simplex could be defined in terms of "(Face, Face, Face, Face)"
extractFaces::SetSimplexPoint -> Face -> Simplex -> SetSimplexFace
extractFaces setPoint f sigma = fromList fs'
    where
        (sA,sB,sC,sD) = setSimplexPointer sigma
        side = simplexAtNormal f
        (a,b,c) = faceRef f
        [d] = [sA, sB, sC, sD] \\ [a,b,c]
        fs = [((a,b,d), c), ((a,d,c), b), ((d,b,c), a)]
        fs' = map toFace fs
        toFace (f, x) = Face ordFace (getside ordFace x)
            where ordFace = fast3DSort f
        getside (ia,ib,ic) i = case isPointAtNormalSideOfFace (setPoint!ia, setPoint!ib, setPoint!ic) (setPoint!i) of
            Just side -> side
            _ -> error "Found degenerate simplex: 4 co-planar points"

extractAllFaces::SetSimplexPoint -> Simplex -> SetSimplexFace
extractAllFaces setPoint sigma =  fromList fsAll'
    where
        (a,b,c,d) = setSimplexPointer sigma
        fsAll = [((a,b,d), c), ((a,d,c), b), ((d,b,c), a), ((a,b,c), d)]
        fsAll' = map toFace fsAll
        toFace (f, x) = Face ordFace (getside ordFace x)
            where ordFace = fast3DSort f
        getside (ia,ib,ic) i = case isPointAtNormalSideOfFace (setPoint!ia, setPoint!ib, setPoint!ic) (setPoint!i) of
            Just side -> side
            _ -> error "Found degenerate simplex: 4 co-planar points"


pointSetPartition::(Box, Box) -> SetSimplexPoint -> [SimplexPointer] -> ([SimplexPointer], [SimplexPointer])
pointSetPartition pairBox setPoint = splitInBox ([],[])
    where
            splitInBox (p1,p2) [] = (p1,p2)
            splitInBox (p1,p2) (x:xs) = case whereIsPoint pairBox (setPoint!x) of
                B1 -> splitInBox (x:p1,p2) xs
                B2 -> splitInBox (p1,x:p2) xs
                B1B2Plane -> splitInBox (x:p1,p2) xs        -- when at plane go to B1. Convetion
                _ -> splitInBox (p1,p2) xs


whereIsPoint::(Box, Box) -> Point -> Position
whereIsPoint (box1,box2) p = case (isInBox box1 p, isInBox box2 p) of
    (True, True) -> B1B2Plane
    (True, False) -> B1
    (False, True) -> B2
    _ -> None
    where
        isInBox box (Vec3D x y z) = between (xMin box) (xMax box) x
                                    && between (yMin box) (yMax box) y
                                    && between (zMin box) (zMax box) z
        between min max x
            | min < max = (x >= min) && (max >= x)   -- >= will get points on the edge of the box and store if P1 those are on the commun face
            | min > max = (x <= min) && (max <= x)
            | otherwise = error ("Zero size box: " ++ show (box1, box2))


makeFirstSimplex::SetSimplexPoint -> Plane -> [SimplexPointer] -> [SimplexPointer] -> [SimplexPointer] -> Maybe Simplex
makeFirstSimplex setPoint alpha p1 p2 p = tryMaybe ((getFace True) >>= (makeSimplex setPoint p))((getFace False) >>= (makeSimplex setPoint p))
    where
        distPointToPlane i = ((planeNormal alpha) `dot` (setPoint!i)) - (planeDist alpha)
        distPointToPoint a b =  norm $ (setPoint!b)-(setPoint!a)
        getFace side = do
            (d1,a1) <- findClosestButZero distPointToPlane p1
            (d2,a2) <- findClosestButZero distPointToPlane p2
            (_, b) <-  if d1 < d2 then findClosestButZero (distPointToPoint a1) p2
                                  else findClosestButZero (distPointToPoint a2) p1
            let a = if d1 < d2 then a1 else a2
                cleanP = filter (\i -> (i /= a) && (i /= b)) p  -- remove points from face to avoid get 0.0 in findMin

            (_, c) <- findClosestButZero (\i -> getRadiusCircumCircle (setPoint!a) (setPoint!b) (setPoint!i)) cleanP
            return (Face (a, b, c) side)

tryMaybe::Maybe a -> Maybe a -> Maybe a
tryMaybe ma mb = if isJust ma then ma else tryMb
    where tryMb = if isJust mb then mb else Nothing


makeSimplex::SetSimplexPoint -> [SimplexPointer] -> Face -> Maybe Simplex
makeSimplex setPoint p (Face (a, b, c) prevSide) = findMinRadius >>= buildFace
    where
        cleanP = filter (\i -> (i /= a) && (i /= b) && (i /= c)) p  -- remove points from face to avoid get 0.0 in findMin
        findMinRadius = findMinimunButZero getRadius $ filter isSideOk cleanP
        buildFace (_, d) = return $ test $ Simplex {circumSphereCenter = center, setSimplexPointer=(a,b,c,d)}
            where (_, center) = getCircumSphere facePoints (setPoint!d)
        facePoints = getSortedPlanePoints setPoint (a, b, c)

        getRadius i = case isSideOpsite centerSide pointSide of
            (Just True) -> (-radius)
            _ -> radius
            where
                (radius, center) = getCircumSphere facePoints (setPoint!i)
                centerSide = isPointAtNormalSideOfFace facePoints center
                pointSide = isPointAtNormalSideOfFace facePoints (setPoint!i)

        isSideOk i = case isSideOpsite pointSide (Just prevSide) of
            (Just faceside) -> faceside
            _ -> False
            where   pointSide = isPointAtNormalSideOfFace facePoints (setPoint!i)

        isSideOpsite = liftM2 func
            where func a b = (a || b) && not (a && b)

        test = testSimplex setPoint p



testSimplex::SetSimplexPoint -> [SimplexPointer] -> Simplex -> Simplex
testSimplex setPoint p sigma
    | isOk =  sigma
    | otherwise = error $ "Trying improper DT." ++ show (pA, pB, pC)
    where
        (a,b,c,d) = setSimplexPointer sigma
        isOk = and $ map distP cleanP
        distP i = radius  < norm (setPoint!i - centre)
        pA = (setPoint!a)
        pB = (setPoint!b)
        pC = (setPoint!c)
        pD = (setPoint!d)
        (radius, centre) = getCircumSphere (pA, pB, pC) pD
        cleanP = filter (\i -> (i /= a) && (i /= b) && (i /= c) && (i /= d)) p  -- remove points from face to avoid get 0.0 in findMin


getSortedPlanePoints::SetSimplexPoint -> (SimplexPointer,SimplexPointer,SimplexPointer) -> (Point,Point,Point)
getSortedPlanePoints setPoint f = (setPoint!ia, setPoint!ib, setPoint!ic)
    where (ia, ib, ic) = fast3DSort f


-- Caution! the order of a b and c points change the normal direction. Use the function getSortedPlanePoints to sort the points first
isPointAtNormalSideOfFace::(Point,Point,Point) -> Point -> Maybe Bool
isPointAtNormalSideOfFace (a,b,c) x
    | d > 0 = Just True
    | d < 0 = Just False
    | n == 0 = error ("Degenerated face:" ++ show (a,b,c))
    | otherwise = Nothing
    where
        n = pack $ (unpack (b - a)) `cross` (unpack (c - a))
        d = n `dot` (x - a)

-- TODO verify correctness
genPlane::Box -> (Plane, Box, Box)
genPlane box
    | (deltaX > (max deltaY deltaZ)) = (Plane (Vec3D 1 0 0) halfX, box { xMax = halfX }, box { xMin = halfX })
    | ((max deltaX deltaY) < deltaZ) = (Plane (Vec3D 0 0 1) halfZ, box { zMax = halfZ }, box { zMin = halfZ })
    | otherwise =                      (Plane (Vec3D 0 1 0) halfY, box { yMax = halfY }, box { yMin = halfY })
    where
        xBot = xMin box
        xUp = xMax box
        yBot = yMin box
        yUp = yMax box
        zBot = zMin box
        zUp = zMax box
        deltaX = xUp - xBot
        deltaY = yUp - yBot
        deltaZ = zUp - zBot
        halfX = (xUp+xBot)/2
        halfY = (yUp+yBot)/2
        halfZ = (zUp+zBot)/2


-- | Performance can be improve by removing the duplicate call to "func" in "dropZero" and the first "(func x, x)"
-- | OBS: Not the closest to zero. In that case
findClosestButZero::(SimplexPointer -> Double) -> [SimplexPointer] -> Maybe (Double, SimplexPointer)
findClosestButZero func = findMinimunButZero (abs.func)


-- | Performance can be improve by removing the duplicate call to "func" in "dropZero" and the first "(func x, x)"
-- | OBS: Not the closest to zero. In that case
findMinimunButZero::(SimplexPointer -> Double) -> [SimplexPointer] -> Maybe (Double, SimplexPointer)
findMinimunButZero func p = case pStartWithNoZero of
    [] -> Nothing
    (x:xs) -> Just $ foldl' (\pair i -> foldMaybe pair (func i, i)) (func x, x) xs
    where
        pStartWithNoZero = dropWhile dropZero p
        dropZero = (flip$(==).func) 0
        foldMaybe new@(n, i) old@(nOld, iOld)
                | n == 0 = old
                | n > nOld = old
                | n < nOld = new
                | otherwise = error $ "Multiple points on circle or sphere! " ++ show new


isInBox::Box -> Point -> Bool
isInBox box (Vec3D x y z) = (xMax box > x && xMin box < x) &&
                            (yMax box > y && yMin box < y) &&
                            (zMax box > z && zMin box < z)


getBox::SetSimplexPoint -> [SimplexPointer] -> Box
getBox _ [] = Box 0 0 0 0 0 0
getBox setPoint (x:xs) = Box {xMin=xBot, xMax=xUp, yMin=yBot,
                              yMax=yUp, zMin=zBot, zMax=zUp}
    where
        ((xBot,xUp),(yBot,yUp),(zBot,zUp)) = foldl' maxMin box0 xs
        (Vec3D x0 y0 z0) = setPoint!x
        box0 = ((x0,x0), (y0,y0), (z0,z0))
        maxMin ((xBot,xUp),(yBot,yUp),(zBot,zUp)) i = (nexX,nexY,nexZ)
            where
                nexX = (min xBot x, max xUp x)
                nexY = (min yBot y, max yUp y)
                nexZ = (min zBot z, max zUp z)
                (Vec3D x y z) = setPoint!i


getRadiusCircumCircle::Point -> Point -> Point -> Double
getRadiusCircumCircle p1 p2 p3 = abs r
    where   a = p1-p3
            b = p2-p3
            d = norm ((unpack $ a) `cross` (unpack $ b))
            r =  (norm a)*(norm b)*(norm $ a -b)/(2*d)

getCircumSphere::(Point, Point, Point) -> Point -> (Double, Point)
getCircumSphere (a, b, c) d = (radius, center)
    where
        radius = abs $ (norm q)/div
        center = a + (q/(pack $ vec div))

        ref = a
        deltaA = unpack (b - ref)
        deltaB = unpack (c - ref)
        deltaC = unpack (d - ref)
        crossB_C = (deltaB `cross` deltaC)
        crossC_A = (deltaC `cross` deltaA)
        crossA_B = (deltaA `cross` deltaB)
        x = ((norm2 deltaA) * crossB_C)
        w = ((norm2 deltaB) * crossC_A)
        t = ((norm2 deltaC) * crossA_B)
        norm2 x = vec n
            where n = dot x x
        div = 2 * (deltaA `dot` crossB_C)
        q = pack (x+w+t)


fast3DSort::(SimplexPointer, SimplexPointer, SimplexPointer) -> (SimplexPointer, SimplexPointer, SimplexPointer)
fast3DSort (a, b, c)
   | (a >= b) && (b >= c) = (a, b, c)
   | otherwise = (a', b', c')
    where
        minab = min a b
        maxab = max a b
        a' = max (maxab) c
        b' = max (min (maxab) c) (minab)
        c' = min (minab) c
