{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Geometry
    ( QuadFace (..), TriFace (..)
    , Point (..), Vertex (..), Normal (..)
    , Face (..)
    , Pos3D (..)
    , Norm3D (..)
    , Vec3D (..)
    , UnitPlane (..)
    , planarAngle, faceNormal, vecAngle, vecAngle', midPointOf, centerOf
    , lineSegmentsIntersect, quadSelfIntersects
    , mostVariedPlane
    ) where

import Relude
import Data.List ((!!), intersect)

-- | A discrete triangular mesh face defined by its three vertices.
data TriFace p = TriFace p p p
    deriving (Show, Eq, Ord)

-- | A discrete quadrilateral mesh face defined by its four vertices.
data QuadFace p = QuadFace p p p p
    deriving (Show, Eq, Ord)

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

class Pos3D n p => Face n p f | f -> p where

    -- | Yields all `Pos3D` points defining the `Face`, this may be any number of coplanar points,
    -- but must be at least three.
    facePoints :: f -> [p]

    faceEdges :: f -> [(p, p)]
    faceEdges f = makeEdges fPoints
      where
        fPoints = facePoints f ++ maybeToList (viaNonEmpty head (facePoints f))
        makeEdges (p : p' : ps) = (p, p') : makeEdges (p' : ps)
        makeEdges _ = []

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

    -- | True if the two faces share a side.
    faceIsAdjacent :: Eq p => f -> f -> Bool
    faceIsAdjacent f f' = not $ null (fEdges `intersect` faceEdges f')
      where
        fEdges = faceEdges f ++ fmap (\(a, b) -> (b, a)) (faceEdges f)

    commonEdge :: Eq p => f -> f -> Maybe (p, p)
    commonEdge f f' = viaNonEmpty head (fEdges `intersect` faceEdges f')
      where
        fEdges = faceEdges f ++ fmap (\(a, b) -> (b, a)) (faceEdges f)

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
        x' = ycomp v * zcomp v0 - zcomp v * ycomp v0
        y' = zcomp v * xcomp v0 - xcomp v * zcomp v0
        z' = xcomp v * ycomp v0 - ycomp v * xcomp v0

    vecDot :: v -> v -> n
    vecDot v v0 = xcomp v * xcomp v0 + ycomp v * ycomp v0 + zcomp v * zcomp v0

    magnitude :: v -> n
    magnitude v = sqrt $ (xcomp v ** 2) + (ycomp v ** 2) + (zcomp v ** 2)

-- | Yields the midpoint of the two given vectors.
midPointOf :: Vec3D n v => v -> v -> v
midPointOf a b = vecScale (vecAdd a b) 0.5

-- | Yields the midpoint of the two given vectors.
centerOf :: Vec3D n v => NonEmpty v -> v
centerOf (p :| ps) = vecScale (foldr vecAdd p ps) (1.0 / fromIntegral (1 + length ps))

-- | Give the angle between two `Face`'s respective planes, in radians.
planarAngle :: Face n p f => f -> f -> n
planarAngle f f0 = vecAngle (faceNormal f) (faceNormal f0)

-- | Gives the angle between two `Vec3D` vectors, in radians.
vecAngle :: Vec3D n v => v -> v -> n
vecAngle v v0 = abs $ acos $ (v `vecDot` v0) / (magnitude v * magnitude v0)

vecAngle' :: Vec3D n v => v -> v -> n
vecAngle' v v0 = acos $ (v `vecDot` v0) / (magnitude v * magnitude v0)

quadSelfIntersects :: (Vec3D n v, Ord n) => QuadFace v -> Bool
quadSelfIntersects (QuadFace a b c d)
    = lineSegmentsIntersect (a, b) (c, d)
    || lineSegmentsIntersect (b, c) (d, a)

lineSegmentsIntersect :: (Vec3D n v, Ord n) => (v, v) -> (v, v) -> Bool
lineSegmentsIntersect (a, b) (c, d) = case mostVariedPlane [a, b, c, d] of
    XYPlane -> segmentsIntersect2
        (xcomp a, ycomp a) (xcomp b, ycomp b) (xcomp c, ycomp c) (xcomp d, ycomp d)
    YZPlane -> segmentsIntersect2
        (ycomp a, zcomp a) (ycomp b, zcomp b) (ycomp c, zcomp c) (ycomp d, zcomp d)
    XZPlane -> segmentsIntersect2
        (xcomp a, zcomp a) (xcomp b, zcomp b) (xcomp c, zcomp c) (xcomp d, zcomp d)

-- | Whether or not two line segments, in 2D, meet in the middle of either segment.
segmentsIntersect2 :: (Ord n, Fractional n) =>(n, n) -> (n, n) -> (n, n) -> (n, n) -> Bool
segmentsIntersect2 (x1, y1) (x2, y2) (x3, y3) (x4, y4)
    = (0 < t && t < 1) || (0 < u && u < 1)
  where
    tNum = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    tDen = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    t = tNum / tDen
    uNum = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3))
    uDen = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    u = uNum / uDen

mostVariedPlane :: (Vec3D n v, Ord n) => [v] -> UnitPlane
mostVariedPlane (firstV:vs)
    | leastVariedAxis == dx = YZPlane
    | leastVariedAxis == dy = XZPlane
    | otherwise = XYPlane
  where
    ((xMin', xMax'), (yMin', yMax'), (zMin', zMax'))
        = foldr (\v ((xMin, xMax), (yMin, yMax), (zMin, zMax))
            -> let
                x = xcomp v
                y = ycomp v
                z = zcomp v
            in ((min xMin x, max xMax x), (min yMin y, max yMax y), (min zMin z, max zMax z))
            )
            ( (xcomp firstV, xcomp firstV)
            , (ycomp firstV, ycomp firstV)
            , (zcomp firstV, zcomp firstV)
            ) vs
    dx = xMax' - xMin'
    dy = yMax' - yMin'
    dz = zMax' - zMin'
    leastVariedAxis = min (min dx dy) dz
mostVariedPlane _ = XYPlane

-- | Calculates the `Normal` normal vector to the plane of the given `Face`.
faceNormal :: Face n p f => f -> Normal n
faceNormal face = Normal (xcomp norm) (ycomp norm) (zcomp norm)
  where
    (a, b, c) = facePoints3 face
    ba = b `vecSub` a
    ca = c `vecSub` a
    norm = ba `vecCross` ca

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
