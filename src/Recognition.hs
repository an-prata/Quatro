{-# LANGUAGE NoImplicitPrelude #-}

module Recognition
    ( Mesh (..), Outline (..)
    , DMeshEdge (..), DMesh
    , describeMesh, structureMesh, collectEdges
    , makeOutline, strandsOf, straightenStrand, strandQuads
    , meshSize
    ) where

import Relude
import Geometry
import Data.List (delete, nub)

-- | Describes a 3D model's mesh not only by its `Face`s, but also the way in which those `Face`s
-- connect. A properly structured `Mesh` is one in which all branches off of a node are `Faces`
-- adjacent to said node. This results in a tree in which all connection indicate adjacency or a
-- shared edge.
data Mesh f = Mesh f [Mesh f]
    deriving (Eq, Ord, Show)

-- | A branching set of line segments which, if interpereted as lines, would outline an object in 3D
-- space.
data Outline p = Outline (p, p) [Outline p]
    deriving (Eq, Ord, Show)

-- | A edge between mesh `Face`s.
data DMeshEdge n p f = DMeshEdge n (p, p) (DMesh n p f)
    deriving (Eq, Ord, Show)

-- | A mesh `Face` linked by `MMeshEdge`s.
data DMesh n p f = DMesh f [DMeshEdge n p f]
    deriving (Eq, Ord, Show)

-- | Creates a `List` of `Outline`s which describe the given `Mesh`. This function seeks to minimize
-- the number of `Outline`s used.
makeOutline :: (Eq p, Ord n) => n -> DMesh n p f -> [Outline p]
makeOutline angle
    = mapMaybe (pruneOutline <=< viaNonEmpty (fst . structureOutline))
    . tails
    -- . (fmap reverse . tails . reverse)
    . mapMaybe (\(DMeshEdge n (a, b) _) -> if n >= angle then Just (a, b) else Nothing)
    . collectEdges

-- | Structures an `Outline` tree around the given two-tuple edge. This function seeks to minimize
-- redundancy and in doing so will create a left heavy tree.
structureOutline :: Eq p => NonEmpty (p, p) -> (Outline p, [(p, p)])
structureOutline (edge :| edges) = foldr structureBranch (Outline edge [], edges) adjacent
  where
    adjacent = filter (edge `meetsEdge`) edges
    structureBranch adj (Outline e bs, re)
        = let (branch, re') = structureOutline (adj :| delete adj re)
        in (Outline e (branch : bs), re')

-- | Removes branches of the given `Outline` which do not form closed loops with the edge at the root
-- of the `Outline` tree.
pruneOutline :: Eq p => Outline p -> Maybe (Outline p)
pruneOutline outline@(Outline top _) = pruneFor top outline
  where
    connectsTo e (Outline e' ols) = (e /= e' && e `meetsEdge` e') || ((e `connectsTo`) `any` ols)

    pruneFor e ol@(Outline e' ols) | e `connectsTo` ol
        = Just $ Outline e' (mapMaybe (pruneFor e) ols)
    pruneFor _ _ = Nothing

strandsOf :: Outline p -> [NonEmpty (p, p)]
strandsOf (Outline top []) = [top :| []]
strandsOf (Outline top ols) = fmap ((top :|) . toList) (concatMap strandsOf ols)

straightenStrand :: Eq p => NonEmpty (p, p) -> [p]
straightenStrand es = nub $ concatMap (\(a, b) -> [a, b]) es

strandQuads' :: (Vec3D n p, RealFloat n) => [p] -> [QuadFace p]
strandQuads' [a, b, c, d]
    -- Test for coplanarity
    | abs (((b `vecSub` a) `vecCross` (d `vecSub` a)) `vecDot` (c `vecSub` a)) < 0.001
        = let q = windQuad a b c d
        in [q]
        -- in [q | not (quadSelfIntersects q)]
    | otherwise = []
strandQuads' _ = []

windQuad :: (Vec3D n p, RealFloat n) => p -> p -> p -> p -> QuadFace p
windQuad a b c d = QuadFace i j k l
  where
    center = (a `vecAdd` b `vecAdd` c `vecAdd` d) `vecScale` 0.25
    plane = mostVariedPlane [a, b, c, d]
    [i, j, k, l] = case plane of
        XYPlane -> sortOn (\v ->
            let v' = v `vecSub` center
            in atan2 (ycomp v') (xcomp v')
            ) [a, b, c, d]
        XZPlane -> sortOn (\v ->
            let v' = v `vecSub` center
            in atan2 (zcomp v') (xcomp v')
            ) [a, b, c, d]
        YZPlane -> sortOn (\v ->
            let v' = v `vecSub` center
            in atan2 (zcomp v') (ycomp v')
            ) [a, b, c, d]

strandQuads :: (Vec3D n p, Eq p, RealFloat n) => [(p, p)] -> [QuadFace p]
strandQuads ps = concatMap strandQuads'
    $ mapMaybe (viaNonEmpty straightenStrand)
    $ filter ((== 4) . length)
    $ subsequences ps

-- | `True` if the two edges meet at either of their endpoints.
meetsEdge :: Eq p => (p, p) -> (p, p) -> Bool
meetsEdge (a, b) (a', b') = b == a' || a == b' || a == a' || b == b'

-- | Yields a `DMeshFace` given a `Mesh`, and creates `DMeshEdge`s with the angle betwen the faces
-- connected by that edge.
describeMesh :: (Eq p, Face n p f) => Mesh f -> Maybe (DMesh n p f)
describeMesh (Mesh f ms) = DMesh f <$> describeEdges f ms

describeEdges :: (Eq p, Face n p f) => f -> [Mesh f] -> Maybe [DMeshEdge n p f]
describeEdges parent = mapM (describeEdge parent)

describeEdge :: (Eq p, Face n p f) => f -> Mesh f -> Maybe (DMeshEdge n p f)
describeEdge parent mesh@(Mesh f _) = describeMesh mesh
    >>= \m -> fmap (\e -> DMeshEdge (planarAngle parent f) e m) (commonEdge parent f)

-- | Deconstructs a described mesh (`DMeshFace`) and yields all of its contained `DMeshEdge`s.
collectEdges :: DMesh n p f -> [DMeshEdge n p f]
collectEdges (DMesh _ es) = concatMap (\edge@(DMeshEdge _ _ f) -> edge : collectEdges f) es

-- | Take a `NonEmpty` list of `Face`s and structure/organize them in a `Mesh` tree. This function
-- will yield a two-tuple in which the first item is the newly structured `Mesh` and the second item
-- is `List` of "remainder" `Face`s, which could not be incorperated into the `Mesh` tree.
--
-- The resulting `Mesh` tree will be left heavy, in the sense that the left-most branch will be
-- maximized, having branches appended to it until no more can, and after the the function moves
-- from left to right, resulting in all branches to the right being less "filled out" that the ones
-- to the left.
structureMesh :: (Eq p, Eq f, Face n p f) => NonEmpty f -> (Mesh f, [f])
structureMesh (face :| faces) = foldr structureBranch (Mesh face [], faces) adjacent
  where
    adjacent = filter (faceIsAdjacent face) faces
    structureBranch adj (Mesh f bs, re)
        = let (branch, re') = structureMesh (adj :| delete adj re)
        in (Mesh f (branch : bs), re')

-- | Finds the size of the `Mesh` as the number of faces in it, including duplicates.
meshSize :: Mesh f -> Int
meshSize (Mesh _ ms) = 1 + sum (meshSize <$> ms)
