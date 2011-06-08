
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module DelaunayReverseOnion (
deWall,
initDeWallState,
Simplex(circumSphereCenter,setSimplexPointer),
SimplexPointer(..),
SetSimplexPoint,
Face,
DeWallSets(setPoint),
faceRef,
Box(xMax,xMin,yMax,yMin,zMax,zMin,Box),
getBox,
isInBox,
onlySimpleInBox,
) where

import Prelude hiding (null)
import Data.Vec hiding (map, length, fromList, fold, get)
import Data.List (map, foldl', filter, head, (\\))
import Data.Array.IArray (Array, Ix, (!))
import Data.Set ( Set, deleteFindMax, member, empty, null
                , delete, insert, fromList, fold, elems, union )
import Data.Maybe
import Monad (liftM, liftM2, foldM)
import Data.IORef
import Control.Monad.State
import Control.Parallel
import Data.Random
import Data.Random.RVar
import System.Random.Mersenne.Pure64

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x


-- >>>>>>>>>>> Type definitions <<<<<<<<<<<<<<<<<<<

-- | Define a point in 3D (x,y,z)
type Point = Vec3D

-- | Group all the points in an array that will be acess by a pointer. It saves memory.
--   It uses a immutable array, so it is not a updatable array.
type SetSimplexPoint = Array SimplexPointer Point

-- | Create a container to store processed faces.
--   Used for active faces on both half-spaces (box1 & box2) and for faces intersect by the plane
type SetSimplexFace = Set Face

-- | Create pointer to access the a point possiton in the array.
newtype SimplexPointer = SimplexPointer Int deriving (Eq, Ord, Num, Ix, Real, Enum, Integral, Show)

-- | Data structure for the basic element of 3D Delaunay Triangulation (DT).
data Simplex = Simplex { circumSphereCenter::Point
                       , setSimplexPointer::(SimplexPointer, SimplexPointer, SimplexPointer, SimplexPointer) } deriving (Show)

-- | Define the a plane that dissect the space in two half-space.
--   All the elements of DT will be constructed recursively over this plane.
--   Plane is represented by a vector from oring (0,0,0) and a scalar such that k*(a,b,c)
--   is the closest point in the plane to the oring
data Plane = Plane { planeNormal::Vec3D
                   , planeDist::Double } deriving (Show, Eq)

-- | Define boxes to represent the space and half-spaces
data Box = Box { xMax::Double
               , xMin::Double
               , yMax::Double
               , yMin::Double
               , zMax::Double
               , zMin::Double } deriving (Show)

-- | Define possible possitions of the elements for the 1st half-space (Box1=B1),
--   2nd (Box2=B2) and intersect by the plane (B1B2Plane).
data Position = B1 | B2 | B1B2Plane | None deriving Show

-- | Create a structure for face (Triangles in the case of 3D DT) and store the orientation
--   of the face related to the previous generated simplex that create the face.
data Face = Face { faceRef::(SimplexPointer, SimplexPointer, SimplexPointer)
                 , simplexAtNormal::Bool } deriving Show

-- Instances for Face
instance Ord Face where
    compare a b = compare ((fast3DSort.faceRef) a) ((fast3DSort.faceRef) b)

instance Eq Face where
    x == y = compare x y == EQ


-- | Group the data that must be update along the computation (State).
--   Use of state monad will make it clear and keep the purity of the code.
data DeWallSets = DeWallSets { aflAlpha, aflBox1, aflBox2::SetSimplexFace
                             , setPoint::SetSimplexPoint } deriving (Show)

type DeWallState = State DeWallSets



getaPointInBox::IORef PureMT -> RVar Double -> Box -> IO Vec3D
getaPointInBox gen f (Box{xMax,xMin,yMax,yMin,zMax,zMin}) = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    a <- sampleFrom gen (uniform xMin xMax)
    b <- sampleFrom gen (uniform yMin yMax)
    c <- sampleFrom gen (uniform zMin zMax)
    return $ Vec3D a b c


getaPointAtDist::IORef PureMT -> RVar Double -> Vec3D -> IO Vec3D
getaPointAtDist gen f ref = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    dist <- sampleFrom gen f
    dx   <- sampleFrom gen stdUniform
    dy   <- sampleFrom gen stdUniform
    dz   <- sampleFrom gen stdUniform
    let norm = sqrt (dx^2 + dy^2 + dz^2)
        k = dist/norm
        displace = Vec3D (dx*k) (dy*k) (dz*k)
    return (ref + displace)



getaPointFromFace::IORef PureMT -> RVar Double -> (Vec3D, Vec3D, Vec3D) -> Bool -> IO Vec3D
getaPointFromFace gen f tri@(pa, pb, pc) simplexNormal = do
    -- To avoid repetition on the pseudo-random generator, use one external gen
    -- wrapped in an StateMonad. Or for internal gen use : "gen <- getRandomGen"
    dist <- sampleFrom gen f
    let centroidFace = (pa + pb + pc)/3
        n = getNormalFace tri
        -- TODO solve for (-) dist and 0 dist
        displace = if simplexNormal then n*(pack.vec $ (-1)*(abs dist)) else n*(pack.vec $ abs dist)
    return (centroidFace + displace)



onlySimpleInBox::Box -> [Simplex] -> [Simplex]
onlySimpleInBox box ls = filter ((isInBox box).circumSphereCenter) ls

initDeWallState setPoint = DeWallSets { aflAlpha=empty, aflBox1=empty
                                      , aflBox2=empty, setPoint }

mergeState::DeWallSets -> DeWallSets -> DeWallSets
mergeState s1 s2 = DeWallSets { aflAlpha = uniState aflAlpha s1 s2 , aflBox1 = uniState aflBox1 s1 s2, aflBox2 = uniState aflBox2 s1 s2, setPoint = setPoint s1}
    where uniState f s1 s2 = (f s1) `union` (f s2)

deWall::[SimplexPointer] -> SetSimplexFace -> Box -> DeWallState [Simplex]
deWall p afl box = do
    cleanAFLs
    (p1, p2) <- getNewHalfSpaces p
    if (null afl)
        then do
            (sig, afl) <- getAFL p1 p2 plane
            s <- deWall p afl box
            return (s ++ sig)
        else do
            mapM_ (splitAF (box1, box2)) (elems afl)
            sigma <- getSigma p (box1,box2)
            get >>= recursion p1 p2 sigma
    where
        (plane, box1, box2) = genPlane box
        getNewHalfSpaces p = get >>= \x -> return $ pointSetPartition (box1, box2) (setPoint x) p
        cleanAFLs = modify (\x -> x { aflAlpha=empty, aflBox1=empty, aflBox2=empty })
        recursion p1 p2 sigma deWallSet
            | null afl1 && null afl2 = return sigma
            | (null) afl1 = do
                s <- deWall p2 afl2 box2
                return (s ++ sigma)
            | (null) afl2 = do
                s <- deWall p1 afl1 box1
                return (s ++ sigma)
            | otherwise   = do
                            x <- get
                            -- The state is independant and can discarted as it will be
                            -- ereased at the bigein of the next recursive func call
                            let !s1 = evalState (deWall p1 afl1 box1) x
                            let !s2 = evalState (deWall p2 afl2 box2) x
                            return $ (s1 ++ s2 ++ sigma) --s1 `par` (s2 `pseq` (s1 ++ s2 ++ sigma))
            where   afl1 = aflBox1 deWallSet
                    afl2 = aflBox2 deWallSet



-- Simplex Wall Construction
getSigma::[SimplexPointer] -> (Box,Box) -> DeWallState [Simplex]
getSigma p pairBox = do
    f <- extractFace
    t <- getSimplex p f
    recursion t f
    where
        getFaces f sig  = get >>= \x -> return $ extractFaces (setPoint x) f sig
        getSimplex p f  = get >>=  \x -> return $ makeSimplex (setPoint x) p f
        extractFace     = do
            x <- get
            let (a, b) = deleteFindMax $ aflAlpha x
            put x {aflAlpha = b}
            return a
        recursion t f   = get >>= \x -> if (null.aflAlpha) x
            then return []
            else case t of
                    Just sig -> do
                        getFaces f sig >>= (mapM_ (splitAF pairBox)).elems
                        s <- getSigma p pairBox
                        return (sig:s)
                    _ -> getSigma p pairBox



getAFL ::[SimplexPointer] -> [SimplexPointer] -> Plane -> DeWallState ([Simplex], SetSimplexFace)
getAFL p1 p2 plane = get >>= \x ->
    case makeFirstSimplex (setPoint x) plane p1 p2 (p1 ++ p2) of
        Just sig -> do
            get >>= \x -> return ([sig], extractAllFaces (setPoint x) sig)
        _ -> return ([], empty)


splitAF ::(Box, Box) -> Face -> DeWallState ()
splitAF pairBox f = do
    DeWallSets { aflAlpha=aa, aflBox1=a1, aflBox2=a2, setPoint } <- get
    let (a,b,c) = faceRef f
        upP1    = modify $ \x -> x { aflAlpha=removeFace f aa, aflBox1=updateFace f a1, aflBox2=removeFace f a2 }
        upP2    = modify $ \x -> x { aflAlpha=removeFace f aa, aflBox1=removeFace f a1, aflBox2=updateFace f a2 }
        upAlpha = modify $ \x -> x { aflAlpha=updateFace f aa, aflBox1=removeFace f a1, aflBox2=removeFace f a2 }

    case map (\i -> whereIsPoint pairBox (setPoint!i)) [a,b,c] of
        [B1,B1,B1] -> upP1
        [B2,B2,B2] -> upP2
        [B1,B1B2Plane,B1B2Plane] -> upP1
        [B2,B1B2Plane,B1B2Plane] -> upP2
        [B1B2Plane,B1,B1B2Plane] -> upP1
        [B1B2Plane,B2,B1B2Plane] -> upP2
        [B1B2Plane,B1B2Plane,B1] -> upP1
        [B1B2Plane,B1B2Plane,B2] -> upP2
        [None,_,_] -> return ()
        [_,None,_] -> return ()
        [_,_,None] -> return ()
        _ -> upAlpha


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
isPointAtNormalSideOfFace tri@(a,_,_) x
    | d > 0 = Just True
    | d < 0 = Just False
    | n == 0 = error ("Degenerated face:" ++ show n)
    | otherwise = Nothing
    where
        n = getNormalFace tri
        d = n `dot` (x - a)


-- Caution! the order of a b and c points change the normal direction. Use the function getSortedPlanePoints to sort the points first
getNormalFace::(Point,Point,Point) -> Point
getNormalFace (a,b,c) = pack $ normalize (unpack (b - a)) `cross` (unpack (c - a))


-- TODO verify correctness
genPlane::Box -> (Plane, Box, Box)
genPlane box@Box { xMax, xMin, yMax, yMin, zMax, zMin }
    | (deltaX >= (max deltaY deltaZ)) = (Plane (Vec3D 1 0 0) halfX, box { xMax = halfX }, box { xMin = halfX })
    | ((max deltaX deltaY) <= deltaZ) = (Plane (Vec3D 0 0 1) halfZ, box { zMax = halfZ }, box { zMin = halfZ })
    | otherwise                       = (Plane (Vec3D 0 1 0) halfY, box { yMax = halfY }, box { yMin = halfY })
    where
        deltaX = abs (xMax - xMin)
        deltaY = abs (yMax - yMin)
        deltaZ = abs (zMax - zMin)
        halfX = (xMax + xMin)/2
        halfY = (yMax + yMin)/2
        halfZ = (zMax + zMin)/2


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

