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
data RecursiveGroup = L1 Vec3D [Simplex]
                    | L2 Vec3D [RecursiveGroup]
                    | L3 Vec3D [RecursiveGroup]

instance Show RecursiveGroup where
    show rg = case rg of
        (L3 id x)   -> "\n    L3: " ++ (show id) ++ (show x)
        (L2 id x)   -> "\n        L2: "  ++ (show id) ++ (show x)
        (L1 id x)   -> "\n            L1: " ++ (show id) ++ (foldl' (\acc a -> acc ++
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

calcLevelOne::[Vec3D] -> [Simplex] -> [RecursiveGroup]
calcLevelOne blacklist = (map (\(x,y) -> L1 x y)).sortGroup.(findNode blacklist)

oneLevelDepeer::[Vec3D] -> [RecursiveGroup] -> [RecursiveGroup]
oneLevelDepeer blacklist = map func
    where
    func::RecursiveGroup -> RecursiveGroup
    func rc = case rc of
        (L3 _  _)   -> rc
        (L2 id x)   -> L3 id (oneLevelDepeer (id:blacklist) x)
        (L1 id x)   -> L2 id (calcLevelOne   (id:blacklist) x)

findGrainsTree = (oneLevelDepeer []).(oneLevelDepeer []).(calcLevelOne [])







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


buildFace::RecursiveGroup -> Maybe VoronoiFace
buildFace (L2 id rc ) = sequence rc >>= makePokerFace >>= makeFace
    where
        makeFace e = return $ VoronoiFace id e

        sequence []           = Just []
        sequence (x:xs)  = case x of
            (L1 id ls) ->  case sequence xs of
                Just xs' -> Just (ls:xs')
                _        -> Nothing
            _        -> Nothing
buildFace _ = Nothing


buildGrain::RecursiveGroup -> Maybe VoronoiGrain
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

buildGrain _ = Nothing


makeGrain::[RecursiveGroup] -> [VoronoiGrain]
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

