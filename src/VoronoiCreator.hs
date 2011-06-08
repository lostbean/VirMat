{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module VoronoiCreator (
convert,
getVertexPointArray,
GrainPointer(..),
VoronoiGrain(..),
VoronoiFace(..),
VoronoiEdge(..),
VoronoiVertex(..),
VertexPointer(..),
VertexPointArray(..)

) where

import Data.Array.IArray (listArray)
import DelaunayReverseOnion (Simplex(..), SimplexPointer(..), SetSimplexPoint)
import Data.List as L
import Data.Array.IArray as Arr
import Data.Vec hiding (map, length, head, tail, last)

import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x

-- Type definitions
type VertexPointArray = Array VertexPointer Vertex
type VertexGrainNeighbourArray = Array VertexPointer (GrainPointer, GrainPointer, GrainPointer, GrainPointer)
type Vertex = Vec3D
type Vector = Vec3D
type Point = Vec3D
-- NEWType definitions
newtype VertexPointer = VertexPointer Int deriving (Eq, Ord, Num, Ix, Real, Enum, Integral, Show)
newtype GrainPointer = GrainPointer Int deriving (Eq, Ord, Num, Ix, Real, Enum, Integral, Show)

-- Data definitions
data VertexAroudGrain = VertexAroudGrain {grainRef::GrainPointer,
                                          neighbourVertex::[VertexPointer]} deriving Show

data VoronoiGrain =  VoronoiGrain {grainID::GrainPointer,
                                   faces::[VoronoiFace]} -- deriving Show

data VoronoiFace =   VoronoiFace  {edges::[VoronoiEdge],
                                   centroid::Point,
                                   normal::Vector,
                                   neighbourAroundEdge::(GrainPointer, GrainPointer)} deriving Show

data VoronoiEdge =   VoronoiEdge  {edge::(VoronoiVertex, VoronoiVertex)} deriving Show

data VoronoiVertex = VoronoiVertex {vertexID::VertexPointer,
                                    neighbourAroundVertex::(GrainPointer, GrainPointer, GrainPointer, GrainPointer)} deriving Show



convert::SetSimplexPoint -> [Simplex] -> [SimplexPointer] -> [Maybe VoronoiGrain]
convert setPoint simplex simplexPointerList = map (grainMaker vertex setPoint sigmas) grainNei
    where
        grainList = map simplexToGrainPointer simplexPointerList
        grainNei = findVertexAroundGrains grainList sigmas
        (vertex, sigmas) = splitSimplexToArray simplex


simplexToGrainPointer::SimplexPointer -> GrainPointer
simplexToGrainPointer (SimplexPointer i) = GrainPointer i

grainToSimplexPointer::GrainPointer -> SimplexPointer
grainToSimplexPointer (GrainPointer i) = SimplexPointer i

getVertexPointArray::[Simplex] -> VertexPointArray
getVertexPointArray simplex = vertexPointArray
    where (vertexPointArray, _) = splitSimplexToArray simplex

splitSimplexToArray::[Simplex] -> (VertexPointArray, VertexGrainNeighbourArray)
splitSimplexToArray ps = (vertex, sigmas)
    where
        !vertex = listArray (VertexPointer 1, n) lc
        !sigmas = listArray (VertexPointer 1, n) toGrainPointer
        toGrainPointer = map (\(a,b,c,d) -> (simplexToGrainPointer a,simplexToGrainPointer b,simplexToGrainPointer c,simplexToGrainPointer d)) ls
        n = VertexPointer (length ps)
        (lc, ls) = foldl' (\(c,s) x -> (circumSphereCenter x:c, setSimplexPointer x:s)) ([],[]) ps

findVertexAroundGrains::[GrainPointer] -> VertexGrainNeighbourArray -> [VertexAroudGrain]
findVertexAroundGrains grainList sim = map func grainList
    where
        simList = indices sim
        func i = VertexAroudGrain i (filter (\n -> hasAtLeastVertex (sim!n) i) simList)


grainMaker::VertexPointArray -> SetSimplexPoint -> VertexGrainNeighbourArray -> VertexAroudGrain -> Maybe VoronoiGrain
grainMaker vertexPointArray setPoint grainsAroundVertex vertexAroudGrain = faces >>= (\x -> return $ VoronoiGrain centralGrain x)
    where
        centralGrain = grainRef vertexAroudGrain
        faces::Maybe [VoronoiFace]
        faces = mapM toFace neighbourGrains
            where
                toFace i = buildFace grainsAroundVertex vertexPointArray vertexAroudGrain i >>= makePokerFace i
                makePokerFace i (es, centroid) = return $ VoronoiFace es centroid (normal centralGrain i) (centralGrain, i)
                normal a b = normalize (setPoint!(grainToSimplexPointer b) - setPoint!(grainToSimplexPointer a))
                neighbourGrains = foldl' tryAddAll [] allGrains
                    where
                        allGrains = map (grainsAroundVertex!) (neighbourVertex vertexAroudGrain)
                        tryAddAll xs (a,b,c,d) = tryAdd a $ tryAdd b $ tryAdd c $ tryAdd d xs
                        tryAdd x xs = if x == centralGrain || x `elem` xs then xs else x:xs



buildFace::VertexGrainNeighbourArray -> VertexPointArray -> VertexAroudGrain -> GrainPointer -> Maybe ([VoronoiEdge], Point)
buildFace grainsAroundVertex vertexPointArray vertexAroudGrain p
    | (length xs) >= 3 = getPolygon xs >>= (\a -> return (a, getFaceCentroid xs))
    | otherwise = Nothing
    where
        centralGrain = grainRef vertexAroudGrain
        xs = filter hasCommumEdge (neighbourVertex vertexAroudGrain)
            where
                hasCommumEdge s = hasAtLeastVertex (grainsAroundVertex!s) centralGrain && hasAtLeastVertex (grainsAroundVertex!s) p
        -- Makes a list of edges in a such sequnce and orintentation, e.g. (P0,P1) (P1,P2) .... (Pn,P0)
        -- Two DL simplex will have a edge between the circumsphere center if they share a commun face Dx
        -- The orientation may change between clock and clock-wise but must to be consistent for each face
        getPolygon (w:ws) = makeClosePolygon w ws []
        makeClosePolygon _ [] [] = Nothing
        makeClosePolygon x [] newX = if hasCommumFace (grainsAroundVertex!x) init then Just ((getEdge x i):newX) else Nothing
            where VoronoiEdge (VoronoiVertex i init, _) = last newX
        makeClosePolygon x xs newX = next >>= (\a -> makeClosePolygon a (delete a xs) ((getEdge x a):newX))
            where next = find (\i -> hasCommumFace (grainsAroundVertex!x) (grainsAroundVertex!i)) xs

        getEdge a b = VoronoiEdge (getVertex a, getVertex b)
        getVertex i = VoronoiVertex i (grainsAroundVertex!i)
        getFaceCentroid ls = sumVertex / (fromIntegral k)
            where
                k = length ls
                getVertex x = vertexPointArray!x
                sumVertex = foldl' (\a b -> a + (getVertex b)) (getVertex $ head ls) (tail ls)



hasAtLeastVertex::Ord a => (a, a, a, a) -> a -> Bool
hasAtLeastVertex (s1, s2, s3, s4) x = x==s1 || x==s2 || x==s3 || x==s4

hasCommumFace::Ord a => (a, a, a, a) -> (a, a, a, a) -> Bool
hasCommumFace (s1, s2, s3, s4) (sA, sB, sC, sD) = (uniTest sA + uniTest sB + uniTest sC + uniTest sD) == 3
    where uniTest x = if x==s1 || x==s2 || x==s3 || x==s4 then 1 else 0
