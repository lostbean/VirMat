-----------------------------------------------------------------------------
--
-- Module      :  VoronoiBuilder
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Edgar Gomes
-- Stability   :  dev
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VoronoiBuilder
( convertDT2Voronoi
, findGrainsTree
, Level1(..)
, Level2(..)
, Level3(..)
, VoronoiGrain
, grainCenter
, faces
, VoronoiFace
, faceTo
, edges
) where

-- External modules
import Data.Vec hiding (map, last)
import Data.List (foldl',sortBy,(\\))
import Maybe

-- Internal modules
import Math.DeUni (Simplex, setCellID, circumSphereCenter)

-- | Main Fucntion
convertDT2Voronoi = makeGrain.findGrainsTree


-- |Recursive data tree for constructe grain structure.
--  L3 (level three) - Simplex with a commun vertex, such a vertex is the center of a grain.
--  L2 (level two) - Simplex with a commun vertex - vertex pair, such a a pair defines a face.
--  L3 (level one) - Simplex with a commun vertex, such a vertex is the edge of a face (triple junction).
data Level1 = L1 Vec3D [Simplex]
data Level2 = L2 Vec3D [Level1]
data Level3 = L3 Vec3D [Level2]
   
instance Show Level3 where
  show (L3 id x) = "\n    L3: " ++ (show id) ++ (show x)
instance Show Level2 where
  show (L2 id x) = "\n        L2: "  ++ (show id) ++ (show x)
instance Show Level1 where
  show (L1 id x) = "\n            L1: " ++ (show id) ++ (foldl' (\acc a -> acc ++
                   "\n                     " ++ show a) [] x)

findNode::[Vec3D] -> [Simplex] -> [(Vec3D, Simplex)]
findNode blacklist = foldl' func []
    where
    func acc x =
            let
                (a, b, c, d) = setCellID x
                cellIDlist = [a, b, c, d]
                cleanList = map (\id -> (id, x)) (cellIDlist \\ blacklist)
            in  cleanList ++ acc


sortGroup::(Ord a) => [(a, b)] -> [(a, [b])]
sortGroup = groupOrd.(sortBy comp)
    where
        comp a b = compare (fst a) (fst b)

        groupOrd [] = []
        groupOrd ls@(x:xs) = (fst x, map snd store):groupOrd rest
            where
                (store, rest) = span (\a -> fst x == fst a) ls

calcLevelOne::[Vec3D] -> [Simplex] -> [Level1]
calcLevelOne blacklist = (map (\(x,y) -> L1 x y)).sortGroup.(findNode blacklist)

level1to2::[Vec3D] -> [Level1] -> [Level2]
level1to2 blacklist = map (\(L1 id x) -> L2 id (calcLevelOne (id:blacklist) x))

level2to3::[Vec3D] -> [Level2] -> [Level3]
level2to3 blacklist = map (\(L2 id x) -> L3 id (level1to2 (id:blacklist) x))

findGrainsTree::[Simplex] -> [Level3]
findGrainsTree = (level2to3 []).(level1to2 []).(calcLevelOne [])







data VoronoiGrain = VoronoiGrain
   { grainCenter::Vec3D
   , faces::[VoronoiFace]
   }

data VoronoiFace = VoronoiFace
   { faceTo::Vec3D
   , edges::[Simplex]
   }


instance Show VoronoiGrain where
    show g = "Grain " ++ show (grainCenter g) ++ " :" ++ concatMap showSimplexFaces (faces g) ++ "\n"
        where showSimplexFaces = ("\n\t\t|- " ++).show

instance Show VoronoiFace where
    show f = "face to grain " ++ show (faceTo f) ++ " -> " ++ (show $ map circumSphereCenter (edges f))


buildFace::Level2 -> Maybe VoronoiFace
buildFace (L2 id rc ) = sequence rc >>= makePokerFace >>= makeFace
    where
        makeFace e = return $ VoronoiFace id e

        sequence []           = Just []
        sequence (x:xs)  = case x of
            (L1 id ls) ->  case sequence xs of
                Just xs' -> Just (ls:xs')
                _        -> Nothing

buildGrain::Level3 -> Maybe VoronoiGrain
buildGrain (L3 id fs ) = sequence fs >>= makeGrain
    where
        makeGrain f = return $ VoronoiGrain id f

        sequence []           = Just []
        sequence (x:xs)  = let
            face = buildFace x
                           in
            case face of
            Just f ->
                case sequence xs of
                Just xs' -> Just (f:xs')
                _        -> Nothing
            _        -> Nothing


makeGrain::[Level3] -> [VoronoiGrain]
makeGrain = mapMaybe buildGrain


makePokerFace::[[Simplex]] -> Maybe [Simplex]
makePokerFace xs@(x:xs') = case x of
    [a, _] -> buildSequence a xs >>= testClosure a
    _     -> Nothing
    where
        testClosure _ []     = Nothing
        testClosure x xs
            | (last xs) == x = Just xs
            | otherwise      = Nothing


buildSequence::Simplex -> [[Simplex]] -> Maybe [Simplex]
buildSequence a xs = case lookForNextEdge a xs of
    Found id next [] -> Just [next]
    Found _ next rest ->
        case buildSequence next rest of
            Just js -> Just (next:js)
            _       -> Nothing
    Invalid           -> Nothing
    NotFound id       -> Nothing

data ScanEdge id a  = Invalid | Found id id [a] | NotFound id deriving (Show)


lookForNextEdge::Simplex -> [[Simplex]] -> ScanEdge Simplex [Simplex]
lookForNextEdge p xs@[]           =  NotFound p
lookForNextEdge p xs@(x:xs')      = case x of
    [s1, s2] -> testCandidate s1 s2
    _        -> Invalid
    where
        testCandidate s1 s2
            | s1 == p   =  Found s1 s2 xs'
            | s2 == p   =  Found s2 s1 xs'
            | otherwise =  case lookForNextEdge p xs' of
                Found s1 s2 rest  -> Found s1 s2 (x:rest)
                allRest           -> allRest

