{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Recognition
    ( Mesh (..)
    , MMeshEdge, MMeshFace
    , WeightedEdge (..)
    , connectEdges, collectEdges
    , mapMesh, tileFaces
    , meshLevels, meshSize
    ) where

import Relude
import Geometry
import Data.List (delete)

-- | Describes a 3D model's mesh not only by its `Face`s, but also the way in which those `Face`s
-- connect.
data Mesh f = Mesh f [Mesh f]
    deriving (Eq, Show)

-- | A edge between mesh `Face`s.
data MMeshEdge n p f = MMeshEdge n (p, p) (MMeshFace n p f)
    deriving (Eq, Show)

-- | A mesh `Face` linked by `MMeshEdge`s.
data MMeshFace n p f = MMeshFace f [MMeshEdge n p f]
    deriving (Eq, Show)

-- | An edge and an angle, presumably from the two faces that met on that edge.
data WeightedEdge n p = WeightedEdge n (p, p)
    deriving (Eq, Show)

-- | Connects `WeightedEdge`s together to form `Line`s, each of which must contain segments sourced
-- from `WeightedEdge`s with a weight greater than or equal to the given weight.
connectEdges :: Ord n => [WeightedEdge n p] -> n -> [Line p]
connectEdges edges minWeight = []
  where
    mainEdges = filter (\(WeightedEdge w _) -> w >= minWeight) edges
    ls = fmap (\(WeightedEdge _ (p0, p)) -> lineSingleton p0 p) mainEdges

-- | Map a `Mesh` into a tree structure composed of alternating `MMeshFace`s and `MMeshEdge`s, which
-- describe not only the `Face`s and "shape" of a mesh, but also details the edges in which `Face`s
-- meet.
mapMesh :: (Face n p f, Eq p) => Mesh f -> MMeshFace n p f
mapMesh (Mesh face []) = MMeshFace face []
mapMesh (Mesh face ms) = MMeshFace face $ mapMaybe (mapEdge face . mapMesh) ms

-- | Creates a `MMeshEdge` which links the given `Face` and `MMeshFace` with their shared edge, if
-- that edge exists.
mapEdge :: (Face n p f, Eq p) => f -> MMeshFace n p f -> Maybe (MMeshEdge n p f)
mapEdge f m@(MMeshFace mf _) = (\s -> MMeshEdge (planarAngle f mf) s m) <$> commonFaceEdge f mf

-- | Takes a `NonEmpty` list of `Face`s and structures them in a `Mesh`.
tileFaces :: (Face n p f, Eq f, Eq p) => NonEmpty f -> Mesh f
tileFaces = head . stepMeshTiling . fmap (`Mesh` [])

-- | Takes a `NonEmpty` list of `Mesh`s and maximally connects them to one another, yielding a list
-- of `Mesh`s which do not connect to eachother. This means that if a closed set of `Face`s are
-- given, the yielded list will be of length one.
stepMeshTiling :: (Face n p f, Eq f, Eq p) => NonEmpty (Mesh f) -> NonEmpty (Mesh f)
stepMeshTiling ms@(_ :| []) = ms
stepMeshTiling ms = case find (not . isLead) ms of
    Just leader ->
        let ms' = delete leader (toList ms)
        in stepMeshTiling (linkMesh leader ms' :| ms')
    Nothing -> ms

-- | Produces a new `Mesh` with the same `Face` as the one given, but with all `Mesh`s adjacent to
-- it of the ones given appended to its internal `List`.
linkMesh :: (Face n p f, Eq p) => Mesh f -> [Mesh f] -> Mesh f
linkMesh (Mesh mf mfs) ms = Mesh mf $ mfs ++ filter (\(Mesh f _) -> hasCommonEdge mf f) ms

-- | Yields `True` if the two given `Face`s share an edge with one another.
hasCommonEdge :: (Face n p f, Eq p) => f -> f -> Bool
hasCommonEdge !f !f0 = length (filter (`elem` facePoints f) (facePoints f0)) == 2

-- | Yields a tuple of two points if one, and only one, common edge exists between the two given
-- faces.
commonFaceEdge :: (Face n p f, Eq p) => f -> f -> Maybe (p, p)
commonFaceEdge !f !f0 = case filter (`elem` facePoints f) (facePoints f0) of
    [a, b] -> Just (a, b)
    _ -> Nothing

-- | Collects all edges between faces in the given `MMeshFace` and yields `WeightedEdge`s containing
-- the two points forming the edge and the angle between the `Face`s that formed the edge.
collectEdges :: MMeshFace n p f -> [WeightedEdge n p]
collectEdges (MMeshFace _ edges) = (edgeOf <$> edges) ++ concatMap (collectEdges . faceOf) edges

-- | Creates a new `WeightedEdge` from the given `MMeshEdge`.
edgeOf :: MMeshEdge n p f -> WeightedEdge n p
edgeOf (MMeshEdge theta edge _) = WeightedEdge theta edge

-- | Gets the contained `MMeshFace` of the given `MMeshEdge`.
faceOf :: MMeshEdge n p f -> MMeshFace n p f
faceOf (MMeshEdge _ _ f) = f

-- | True if the given `Mesh` if a "lead", meaning that it has one or more recorded adjacent
-- `Mesh`s.
isLead :: Mesh f -> Bool
isLead (Mesh _ []) = False
isLead (Mesh _ _) = True

-- | Yields the number of recursive steps in the `Mesh` tree when taking the left most branch.
meshLevels :: Mesh f -> Int
meshLevels (Mesh _ !ms) = 1 + foldr (\i acc -> acc `max` meshLevels i) 0 ms

-- | Yields the number of faces in the mesh.
meshSize :: Mesh f -> Int
meshSize (Mesh _ []) = 1
meshSize (Mesh _ !ms) = 1 + sum (meshSize <$> ms)
