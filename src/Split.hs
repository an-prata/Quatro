{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Split
    ( Range (..)
    , (...)
    , (..=)
    , boundsOf
    , within
    , rangeToList

    , SplitTree
    , findSplit
    , splitSTreeAt
    , decomposeSTree
    , recomposeSTree

    , Split (..)
    ) where

import Relude

-- | A numeric range, spanning two `Num` instances.
data Range n
    = RangeExclusive n n  -- ^ A range between two `Num`s, not including the second `Num`. [n, m)
    | RangeInclusive n n  -- ^ A range between two `Num`s, inclusing the second `Num`. [n, m]
    deriving (Show, Eq, Ord)

-- | Creates an exclusive range between two `Num`s.
infix 9 ...
(...) :: n -> n -> Range n
n...m = RangeExclusive n m

-- | Creates an inclusive range between two `Num`s.
infix 9 ..=
(..=) :: n -> n -> Range n
n..=m = RangeInclusive n m

-- | Get the `Range`s bounding `Num` instances.
boundsOf :: Range n -> (n, n)
boundsOf (RangeExclusive n m) = (n, m)
boundsOf (RangeInclusive n m) = (n, m)

-- | `True` if a given `Num` instance is within the given `Range`.
within :: Ord n => n -> Range n -> Bool
within idx (RangeExclusive n m) = idx >= n && idx < m
within idx (RangeInclusive n m) = idx >= n && idx <= m

-- | Produce a `List` containing all variants within the described `Range`.
rangeToList :: (Num n, Enum n) => Range n -> [n]
rangeToList (RangeExclusive n m) = [n..m-1]
rangeToList (RangeInclusive n m) = [n..m]

-- | A tree structure for organizing `Split`s which also described the order and manner in which
-- the instance of `Split` had been split, such that it produced the partitions held within the
-- tree.
data SplitTree s n

    -- | A "leaf" of the tree, which holds an instance of `Split` and a `Range` detailing what
    -- portion of the original `Split` the `SplitRange` represents and holds within its instance of
    -- `Split`.
    = SplitRange (Range n) s

    -- | A "branch" of the tree, holding two `SplitRange`s. The structure of these branches should
    -- be indicative of the order/manner in which the original `Split` instance was split in order
    -- to produce the partitions held in the tree.
    | Split (SplitTree s n) (SplitTree s n)
    deriving (Show, Eq)

-- | Create a `SplitTree` with the given tuples of elements and `Range`s.
tupleToSTree :: (s, s) -> (Range n, Range n) -> SplitTree s n
tupleToSTree (a, b) (r1, r2) = Split (SplitRange r1 a) (SplitRange r2 b)

-- | Split the `SplitTree` at the given index, producing either another `SplitTree` or `Nothing`.
splitSTreeAt :: (Ord n, Split n s) => n -> SplitTree s n -> Maybe (SplitTree s n)
splitSTreeAt idx (SplitRange range line)
    | idx `within` range = Just $ tupleToSTree (splitAtS idx line) (splitAtS idx range)
    | otherwise = Nothing
splitSTreeAt idx (Split a b) = whenNothing (splitSTreeAt idx a) (splitSTreeAt idx b)

-- | Find the given `Split` within the given `SplitTree` and return it, or `Nothing` if it cannot be
-- found.
findSplit :: (Eq n, Split n s) => SplitTree s n -> Range n -> Maybe s
findSplit (SplitRange r s) range = if range == r then Just s else Nothing
findSplit (Split left right) range = whenNothing (findSplit left range) (findSplit right range)

-- | Reconstructs the original `Split` from the branches of the given `SplitTree`.
recomposeSTree :: Split n s => SplitTree s n -> s
recomposeSTree (SplitRange _ s) = s
recomposeSTree (Split t1 t2) = recomposeSTree t1 `combineS` recomposeSTree t2

-- | Decomposes the `SplitTree` into a `List` of `Split` instances as detailed by the `SplitTree`s
-- branches. The resulting `List` will be in order, as if reading the tree from left to right, and
-- if combined should produce the original `Split`.
decomposeSTree :: Split s n => SplitTree s n -> [s]
decomposeSTree (SplitRange _ s) = [s]
decomposeSTree (Split t1 t2) = decomposeSTree t1 ++ decomposeSTree t2

-- | Instances of `Split` may be broken down and recombined at specific positions within the `Split`.
-- this position could be an `Integral` index, but could easily be any other type.
class Num n => Split n s | s -> n where

    -- | Split at this given position, not including the position itself. The precise meaning of
    -- "including" the position is not defined here, but for indices should not include that index.
    splitAtS :: n -> s -> (s, s)

    -- | Combine, the first argument should precede the second in the yielded value.
    combineS :: s -> s -> s

instance Split Int [a] where
    splitAtS = splitAt
    combineS = (++)

instance Num n => Split n (Range n) where
    splitAtS i (RangeExclusive n m) = (n...i, i...m)
    splitAtS i (RangeInclusive n m) = (n...i, i..=m)
    combineS a (RangeExclusive _ n) = fst (boundsOf a) ... n
    combineS a (RangeInclusive _ n) = fst (boundsOf a) ..= n
