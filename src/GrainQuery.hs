{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module GrainQuery (
getAreaGrain,
getVolumeGrain,
getFaceArea,
Volume(getVolume),
Area(getArea)
) where

import VoronoiCreator

import Data.Vec (norm, normalize, dot, cross, unpack, pack, Vec3D)
import Data.Array ((!))
import Data.List (foldl')

newtype Area = Area {getArea::Double} deriving (Eq, Ord, Num, Show)
newtype Volume = Volume {getVolume::Double} deriving (Eq, Ord, Num, Show)

getVolumeGrain::VertexPointArray -> VoronoiGrain -> Volume
getVolumeGrain vertexArray grain = Volume $ abs (volume / 3)
    where
        volume = foldl' (\a b -> a + (prismVolume b)) 0 (faces grain)
        prismVolume f = ((normal f) `dot` (centroid f)) * (area f)
        area f = getArea $ getFaceArea vertexArray f

getAreaGrain::VertexPointArray -> VoronoiGrain -> Area
getAreaGrain vertexArray grain = foldl' (\a b -> a + (getFaceArea vertexArray b)) (Area 0) (faces grain)

getFaceArea::VertexPointArray -> VoronoiFace -> Area
getFaceArea vertexArray (VoronoiFace (x:xs) centroid normal (a,b)) = Area $ abs (area / 2)
    where
        -- calculate the "normal area" of a triangle D=OAB, where O = (0,0,0)
        -- It assume that the edge pairs are given in a oriented sequnce, e.g. (P0,P1) (P1,P2) .... (Pn,P0)
        getCrossEdge (VoronoiEdge (a,b)) = (getVertex b) `cross` (getVertex a)
        getVertex (VoronoiVertex x _) = unpack $ vertexArray!x
        area = normal `dot` (pack $ foldl' (\a b -> a + (getCrossEdge b)) (getCrossEdge x) xs)
getFaceArea vertexArray (VoronoiFace [] _ _ _) = error "Can't query empty Voronoi face."






