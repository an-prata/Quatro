{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Geometry
    ( QuadFace (..), TriFace (..)
    , Line, Patch
    , facesFromPatches, facesFromPatch, patchFromLoop, isClosedLoop
    , connectLines, linkLines
    , endpointsOf, lineOf, lineSingleton, lineReverse
    , intersectionsOf
    , minimizePatches
    , Point (..), Vertex (..), Normal (..)
    , Face (..)
    , Pos3D (..)
    , Norm3D (..)
    , Vec3D (..)
    , planarAngle, faceNormal, vecAngle, midPointOf, centerOf
    ) where

import Relude
import Data.Foldable (maximum, minimum)
import Data.List ((!!), nub, minimumBy, (\\))
import GHC.OldList (deleteBy)

-- | A discrete triangular mesh face defined by its three vertices.
data TriFace p = TriFace p p p
    deriving Show

-- | A discrete quadrilateral mesh face defined by its four vertices.
data QuadFace p = QuadFace p p p p
    deriving Show

-- | A plane established by being perpendicular to a unit vector.
data UnitPlane
    = YZPlane -- ^ Perpendicular to the unit vactor î
    | XZPlane -- ^ Perpendicular to the unit vactor ĵ
    | XYPlane -- ^ Perpendicular to the unit vactor k̂
    deriving (Eq, Ord, Enum, Show)

-- | A point in 3D space with both a normal vector and position.
data Point n = Point (Vertex n) (Normal n)
    deriving (Show, Eq, Ord)

-- | A position in 3D space.
data Vertex n = Vertex n n n
    deriving (Show, Eq, Ord)

-- | A 3D normal vector.
data Normal n = Normal n n n
    deriving (Show, Eq, Ord)

-- | A line, as defined by discrete linear segments. Any two adjacent points in the line will
-- connect to form a line segment, the sum of which produce the line.
data Line p = Line p (NonEmpty p)
    deriving (Show, Eq)

-- | A closed loop describing a two dimensional shape made up of straight sides. This shape may or
-- may not reside on a flat plane.
data Patch p = Patch p (Line p)
    deriving (Show, Eq)

class Pos3D n p => Face n p f | f -> p where

    -- | Yields all `Pos3D` points defining the `Face`, this may be any number of coplanar points,
    -- but must be at least three.
    facePoints :: f -> [p]

    -- | Yields three `Pos3D` points which may be used to define the plane of the `Face`.
    facePoints3 :: f -> (p, p, p)
    facePoints3 f = (ps !! 0, ps !! 1, ps !! 2)
      where
        ps = take 3 $ facePoints f

    -- | Yields the `Pos3D` point at the center of the `Face`.
    centerPoint :: f -> p
    centerPoint face = vecScale (foldr vecAdd zero points) (recip $ fromIntegral $ length points)
      where
        points = facePoints face
        zero = newVec3D 0 0 0

-- | A typeclass for types which may provide a three-dimensional normal vector.
class Vec3D n a => Norm3D n a | a -> n where
    xnorm :: a -> n  -- ^ The X component of the normal vector represented by the given `Norm3D`.
    ynorm :: a -> n  -- ^ The Y component of the normal vector represented by the given `Norm3D`.
    znorm :: a -> n  -- ^ The Z component of the normal vector represented by the given `Norm3D`.

-- | A typeclass for types which represent or hold a position in three-dimensional space.
class Vec3D n p => Pos3D n p | p -> n where

    -- | Yields the given `Pos3D`'s coordinates as projected onto the given `UnitPlane`. The items
    -- of the yielded tuple corrospond to the components of the given plane in the same position, 
    -- i.e. `XYPlane` -> (x, y).
    projectTo :: UnitPlane -> p -> (n, n)
    projectTo XYPlane pos = (xcomp pos, ycomp pos)
    projectTo YZPlane pos = (ycomp pos, zcomp pos)
    projectTo XZPlane pos = (xcomp pos, zcomp pos)

    -- | Yields the distance between the two `Pos3D` instances.
    distance :: Pos3D n p' => p -> p' -> n
    distance p0 p = sqrt (dx**2 + dy**2 + dz**2)
      where
        dx = xcomp p - xcomp p0
        dy = ycomp p - ycomp p0
        dz = zcomp p - zcomp p0

class Floating n => Vec3D n v | v -> n where
    xcomp :: v -> n  -- ^ Yields the X component of the given `Vec3D` instance.
    ycomp :: v -> n  -- ^ Yields the Y component of the given `Vec3D` instance.
    zcomp :: v -> n  -- ^ Yields the Z component of the given `Vec3D` instance.

    newVec3D :: n -> n -> n -> v

    vecAdd :: v -> v -> v
    vecAdd v v0 = newVec3D (xcomp v + xcomp v0) (ycomp v + ycomp v0) (zcomp v + zcomp v0)

    vecSub :: v -> v -> v
    vecSub a b = a `vecAdd` vecScale b (-1)

    vecScale :: v -> n -> v
    vecScale v s = newVec3D (xcomp v * s) (ycomp v * s) (zcomp v * s)

    vecCross :: v -> v -> v
    vecCross v v0 = newVec3D x' y' z'
      where
        x' = ycomp v * zcomp v0 + zcomp v * ycomp v0
        y' = zcomp v * xcomp v0 + xcomp v * zcomp v0
        z' = xcomp v * ycomp v0 + ycomp v * xcomp v0

    vecDot :: v -> v -> n
    vecDot v v0 = xcomp v * xcomp v0 + ycomp v * ycomp v0 + zcomp v * zcomp v0

    magnitude :: v -> n
    magnitude v = sqrt $ (xcomp v ** 2) + (ycomp v ** 2) + (zcomp v ** 2)

facesFromPatches :: (Eq p, Vec3D n p) => [Patch p] -> [QuadFace p]
facesFromPatches ls = nub $ concat $ mapMaybe facesFromPatch ls

facesFromPatch :: (Vec3D n p) => Patch p -> Maybe [QuadFace p]
facesFromPatch patch = makeQuads $ toList (pointsOf patch)

-- | Creates quads given an outline of points. Assumes that the start and end point are equal.
makeQuads :: (Vec3D n p) => [p] -> Maybe [QuadFace p]
makeQuads [a, b, c, d, _] = Just [QuadFace a b c d]
makeQuads ps | length ps `mod` 2 == 1 = viaNonEmpty centerOf ps >>= (`facesFromOutline` ps)
makeQuads ps = viaNonEmpty centerOf ps >>= (`facesFromOutline` intersperseMidpoints ps)

-- | Takes a `Line`, presumed to be a closed loop such that its two endpoints are equal, and
-- produces `QuadFaces` that would make up the shape outlines by the given `Line`.
facesFromOutline :: p -> [p] -> Maybe [QuadFace p]
facesFromOutline center [p, p', p''] = Just [QuadFace p p' p'' center]
facesFromOutline _ [p, p', p'', p''', _] = Just [QuadFace p p' p'' p''']
facesFromOutline center (p : p' : p'' : ps)
    = (QuadFace p p' p'' center :) <$> facesFromOutline center (p'' : ps)
facesFromOutline _ _ = Nothing

-- | Places a midpoint between every two point in the given `Line`.
intersperseMidpoints :: (Vec3D n p) => [p] -> [p]
intersperseMidpoints [] = []
intersperseMidpoints [p] = [p]
intersperseMidpoints (p : p' : ps) = p : midPointOf p p' : p' : intersperseMidpoints (p' : ps)

-- | True if the given `Line` starts where it ends.
isClosedLoop :: Eq p => Line p -> Bool
isClosedLoop l = a == b
  where
    (a, b) = endpointsOf l

-- | Maximally connect lines to eachother.
connectLines :: Eq p => [Line p] -> [Line p]
connectLines [] = []
connectLines [l] = [l]
connectLines (l : ls) = case mapMaybe (linkLines l) ls of
    [] -> l : ls
    links -> connectLines links ++ connectLines ls

-- | Append one `Line` onto the other if they have matching endpoints. This function may reverse the
-- direction of one of the given `Line`s, and will not necessarily make a line starting with the
-- first given `Line`.
linkLines :: Eq p => Line p -> Line p -> Maybe (Line p)
linkLines a b
    | a1 == b0 = Just $ lineAppend a b
    | a1 == b1 = Just $ lineAppend a (lineReverse b)
    | a0 == b0 = Just $ lineAppend (lineReverse a) b
    | a0 == b1 = Just $ lineAppend b a
    | otherwise = Nothing
  where
    (a0, a1) = endpointsOf a
    (b0, b1) = endpointsOf b

-- | Appends one `Line` onto another.
lineAppend :: Line p -> Line p -> Line p
lineAppend (Line p0 (p0' :| ps0)) (Line p ps) = Line p0 (p0' :| (ps0 ++ (p : toList ps)))

-- | Yeilds the given `Line` with the order of points reversed
lineReverse :: Line p -> Line p
lineReverse (Line p0 (p :| [])) = Line p (p0 :| [])
lineReverse (Line p0 (p :| [p'])) = Line p' (p :| [p0])
lineReverse (Line p0 (p :| ps)) = case lineOf ps of
    Just l -> lineAppend (lineReverse l) $ Line p (p0 :| [])
    Nothing -> Line p (p0 :| [])

-- | Creates a `Patch` from a `Line` if that `Line` has at least three points and starts where it
-- ends.
patchFromLoop :: Eq p => Line p -> Maybe (Patch p)
patchFromLoop l@(Line p (p' :| (p'' : ps)))
    | isClosedLoop l = Just $ Patch p (Line p' (p'' :| ps))
patchFromLoop _ = Nothing

minimizePatches :: Eq p => [Patch p] -> [Patch p]
minimizePatches [] = []
minimizePatches (p : ps) = minimumBy compareCandidates candidates : minimizePatches nonCandidates
  where
    candidates = p : filter (patchesOverlap p) ps
    nonCandidates = ps \\ candidates
    compareCandidates a b = length (pointsOf a) `compare` length (pointsOf b)

patchesOverlap :: Eq p => Patch p -> Patch p -> Bool
patchesOverlap p p' = length (filter (`notElem` pointsOf p') (toList $ pointsOf p)) > 2

-- | Yields the first and last points of a `Line`.
endpointsOf :: Line p -> (p, p)
endpointsOf (Line p ps) = (p, last ps)

-- | Yields the midpoint of the two given vectors.
midPointOf :: (Vec3D n v) => v -> v -> v
midPointOf a b = vecScale (vecAdd a b) 0.5

-- | Yields the midpoint of the two given vectors.
centerOf :: (Vec3D n v) => NonEmpty v -> v
centerOf (p :| ps) = vecScale (foldr vecAdd p ps) (1.0 / fromIntegral (1 + length ps))

-- | Creates a `Line` from a `List`, yielding `Nothing` if the `List` does not contain two or more
-- points.
lineOf :: [p] -> Maybe (Line p)
lineOf (p0 : p : ps) = Just $ Line p0 (p :| ps)
lineOf _ = Nothing

-- | Creates a single segment `Line` from the two given points.
lineSingleton :: p -> p -> Line p
lineSingleton p0 p = Line p0 (p :| [])

-- | Give the angle between two `Face`'s respective planes, in radians.
planarAngle :: Face n p f => f -> f -> n
planarAngle f f0 = vecAngle (faceNormal f) (faceNormal f0)

-- | Gives the angle between two `Vec3D` vectors, in radians.
vecAngle :: Vec3D n v => v -> v -> n
vecAngle v v0 = abs (v `vecDot` v0) / (magnitude v * magnitude v0)

-- | Calculates the `Normal` normal vector to the plane of the given `Face`.
faceNormal :: Face n p f => f -> Normal n
faceNormal face = Normal (xcomp norm) (ycomp norm) (zcomp norm)
  where
    (a, b, c) = facePoints3 face
    ba = b `vecSub` a
    ca = c `vecSub` a
    norm = ba `vecCross` ca

-- | Finds the positions of all intersections between two `Line`s.
intersectionsOf :: (Ord n, Pos3D n p) => Line p -> Line p -> [Vertex n]
intersectionsOf (Line l0 (l :| ls)) line = case nonEmpty ls of
    Just ls' -> segmentIntersections l0 l line ++ intersectionsOf (Line l ls') line
    Nothing -> segmentIntersections l0 l line

-- | Finds the positions of all intersections of a line segment (as defined by its `Pos3D` 
-- endpoints) with a `Line`.
segmentIntersections :: (Ord n, Pos3D n p) => p -> p -> Line p -> [Vertex n]
segmentIntersections s s' (Line l0 (l :| ls)) = case nonEmpty ls of
    Just ls' -> maybeToList (segmentIntersection s s' l0 l) ++ segmentIntersections s s' (Line l ls')
    Nothing -> maybeToList (segmentIntersection s s' l0 l)

-- | Finds the position of the intersection of two line segments, each defined via two `Pos3D`
-- points, one at each endpoint of the line segment.
segmentIntersection :: (Ord n, Pos3D n p) => p -> p -> p -> p -> Maybe (Vertex n)
segmentIntersection a0 a1 b0 b1
    | dxa == dxb && dya == dyb && dza == dzb = Nothing
    | mostVariedPlane points == XYPlane = intersection2D a0' a1' b0' b1' >>=
        \(x, y) ->
            let s = (x - xcomp a0) / (xcomp a1 - xcomp a0)
                z = zcomp a0 + s * dza
            in Just $ Vertex x y z
    | mostVariedPlane points == YZPlane = intersection2D a0' a1' b0' b1' >>=
        \(y, z) ->
            let s = (y - ycomp a0) / (ycomp a1 - ycomp a0)
                x = xcomp a0 + s * dxa
            in Just $ Vertex x y z
    | mostVariedPlane points == XZPlane = intersection2D a0' a1' b0' b1' >>=
        \(x, z) ->
            let s = (x - xcomp a0) / (xcomp a1 - xcomp a0)
                y = ycomp a0 + s * dya
            in Just $ Vertex x y z
    | otherwise = Nothing
  where
    aLength = a0 `distance` a1
    bLength = b0 `distance` b1
    dxa = (xcomp a1 - xcomp a0) / aLength
    dya = (ycomp a1 - ycomp a0) / aLength
    dza = (zcomp a1 - zcomp a0) / aLength
    dxb = (xcomp b1 - xcomp b0) / bLength
    dyb = (ycomp b1 - ycomp b0) / bLength
    dzb = (zcomp b1 - zcomp b0) / bLength
    points = [a0, a1, b0, b1]
    a0' = projectTo (mostVariedPlane points) a0
    a1' = projectTo (mostVariedPlane points) a1
    b0' = projectTo (mostVariedPlane points) b0
    b1' = projectTo (mostVariedPlane points) b1

-- | Finds the intersection of two two-dimensional line segments, if such a point exists. Takes line
-- segments as two points. The first two tuples define the first line, and the last two tuples
-- define the second line. `Nothing` is yielded if the lines are parallel (infinitely many points in
-- common) or if the intersection would occur outside the bounds of the given points of at least one
-- line.
intersection2D :: (Ord n, Fractional n) => (n, n) -> (n, n) -> (n, n) -> (n, n) -> Maybe (n, n)
intersection2D (ax0, ay0) (ax, ay) (bx0, by0) (bx, by)
    | da == db = Nothing
    | x > max ax0 ax || x < min ax0 ax || x > max bx0 bx || x < min bx0 bx = Nothing
    | otherwise = Just (x, y)
  where
    da = (ax - ax0) / (ay - ay0)
    db = (bx - bx0) / (by - by0)
    x = (by - ay - db*bx0 + da*ax0) / (da - db)
    y = da * (x - ax0) + ay0

-- | Takes a `Foldable` and `Functor` colection of `Pos3D` points and finds the `UnitPlane` in which
-- the points would vary the most if projected. This can be used to establish a plane in which
-- gimbal lock may be avoided.
mostVariedPlane :: (Ord n, Foldable f, Functor f, Pos3D n p) => f p -> UnitPlane
mostVariedPlane f
    | dx < dy && dx < dz = YZPlane
    | dy < dx && dy < dz = XZPlane
    | otherwise = XYPlane
  where
    xs = fmap xcomp f
    ys = fmap ycomp f
    zs = fmap zcomp f
    dx = maximum xs - minimum xs
    dy = maximum ys - minimum ys
    dz = maximum zs - minimum zs

-- | Creates a `NonEmpty` list of points from the given line.
lineToNonEmpty :: Line p -> NonEmpty p
lineToNonEmpty (Line p ps) = p :| toList ps

-- | Creates a `List` of points from the given line.
lineToList :: Line p -> [p]
lineToList (Line p ps) = p : toList ps

-- | Gives the length of a line in points.
lineLength :: Line p -> Int
lineLength = length . lineToList

-- | Gives a list of the points in the given `Patch`. This list's head will be the start/end point,
-- and the start/end point will appear once again at the very end of the `NonEmpty`.
pointsOf :: Patch p -> NonEmpty p
pointsOf (Patch p l) = p :| (lineToList l ++ [p])

-- | Gets the perimeter of a `Patch` in number of piecewise segments.
patchPerimeter :: Patch p -> Int
patchPerimeter = length . pointsOf

instance Pos3D n p => Face n p (TriFace p) where
  facePoints (TriFace a b c) = [a, b, c]

instance Pos3D n p => Face n p (QuadFace p) where
  facePoints (QuadFace a b c d) = [a, b, c, d]

instance Floating n => Pos3D n (Point n) where

instance Floating n => Vec3D n (Point n) where
    xcomp (Point v _) = xcomp v
    ycomp (Point v _) = ycomp v
    zcomp (Point v _) = zcomp v
    newVec3D x y z = Point (Vertex x y z) (Normal x y z)

instance Floating n => Norm3D n (Point n) where
    xnorm (Point _ n) = xnorm n
    ynorm (Point _ n) = ynorm n
    znorm (Point _ n) = znorm n

instance Floating n => Pos3D n (Vertex n) where

instance Floating n => Vec3D n (Vertex n) where
    xcomp (Vertex x _ _) = x
    ycomp (Vertex _ y _) = y
    zcomp (Vertex _ _ z) = z
    newVec3D = Vertex

instance Floating n => Norm3D n (Normal n) where
    xnorm = xcomp
    ynorm = ycomp
    znorm = zcomp

instance Floating n => Vec3D n (Normal n) where
    xcomp (Normal x _ _) = x
    ycomp (Normal _ y _) = y
    zcomp (Normal _ _ z) = z
    newVec3D = Normal

instance Eq p => Eq (TriFace p) where
    (TriFace a b c) == (TriFace a' b' c')
        = (a == a' && b == b' && c == c')
        || (a == b' && b == c' && c == a')
        || (a == c' && b == a' && c == b')

instance Eq p => Eq (QuadFace p) where
    (QuadFace a b c d) == (QuadFace a' b' c' d')
        = (a == a' && b == b' && c == c' && d == d')
        || (a == b' && b == c' && c == d' && d == a')
        || (a == c' && b == d' && c == a' && d == b')
        || (a == d' && b == a' && c == b' && d == c')
