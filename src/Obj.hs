{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Obj
    ( parseObj
    , parseTriFace
    , writeQuads, writeEdges
    ) where

import Relude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Set as Set
import Geometry
import Data.List (elemIndex)

writeEdges :: (Ord a, Show a) => [(Vertex a, Vertex a)] -> Text
writeEdges es = shownPoints `T.append` edges
  where
    points = Set.fromList (concatMap (\(a, b) -> [a, b]) es)
    pointsList = toList points
    shownPoints = T.concat $ fmap (\(Vertex x y z)
        -> "v " `T.append` T.unwords [show x, show y, show z] `T.append` "\n") pointsList
    edges = T.concat $ mapMaybe (\(a, b) -> do
        a' <- elemIndex a pointsList
        b' <- elemIndex b pointsList
        return $ "l " `T.append` T.unwords [show (a' + 1), show (b' + 1)] `T.append` "\n")
        es

writeQuads :: (Ord a, Show a) => [QuadFace (Vertex a)] -> Text
writeQuads qs = shownPoints `T.append` shownFaces
  where
    points = Set.fromList (concatMap (\(QuadFace a b c d) -> [a, b, c, d]) qs)
    pointsList = toList points
    shownPoints = T.concat $ fmap (\(Vertex x y z)
        -> "v " `T.append` T.unwords [show x, show y, show z] `T.append` "\n") pointsList
    faces = mapMaybe (\(QuadFace a b c d) -> do
        a' <- elemIndex a pointsList
        b' <- elemIndex b pointsList
        c' <- elemIndex c pointsList
        d' <- elemIndex d pointsList
        return (QuadFace a' b' c' d'))
        qs
    shownFaces = T.concat $ fmap (\(QuadFace a b c d) -> "f "
        `T.append` T.unwords [show (a + 1), show (b + 1), show (c + 1), show (d + 1)]
        `T.append` "\n") faces

-- | Parse an OBJ file, assuming faces are triangular.
parseObj :: Fractional a => FilePath -> IO [TriFace (Vertex a)]
parseObj fp = do
    t <- TIO.readFile fp
    let vs = parseVertices t
    return $ parseTriFaces vs t

-- | Parse triangular faces.
parseTriFaces :: [Vertex a] -> Text -> [TriFace (Vertex a)]
parseTriFaces vs = mapMaybe (parseTriFace vs) . lines

-- | Parse all vertices in the given text.
parseVertices :: Fractional a => Text -> [Vertex a]
parseVertices = mapMaybe parseVertex . lines

-- | Parses a single triangular face.
parseTriFace :: [Vertex a] -> Text -> Maybe (TriFace (Vertex a))
parseTriFace vs (T.stripPrefix "f " -> Just t) = do
    vertices <- mapM parseFaceVertex (words t)
    a <- vertices !!? 0 >>= (\i -> vs !!? (i - 1))
    b <- vertices !!? 1 >>= (\i -> vs !!? (i - 1))
    c <- vertices !!? 2 >>= (\i -> vs !!? (i - 1))
    return $ TriFace a b c
parseTriFace _ _ = Nothing

-- | Parses the vertex of a face, ignoring possibel texture and normal indices.
parseFaceVertex :: Integral a => Text -> Maybe a
parseFaceVertex text = case parseInt <$> T.splitOn "/" text of
    (Just v : _) -> Just v
    _ -> Nothing

-- | Parse a single OBJ vertex.
parseVertex :: Fractional a => Text -> Maybe (Vertex a)
parseVertex (T.stripPrefix "v " -> Just t) = do
    components <- mapM parseFraction (words t)
    x <- components !!? 0
    y <- components !!? 1
    z <- components !!? 2
    return $ Vertex x y z
parseVertex _ = Nothing

-- | Parse a decimal, yielding `Nothing` on any extranious characters.
parseFraction :: Fractional a => Text -> Maybe a
parseFraction t = case TR.rational (T.strip t) of
    Right (x, "") -> Just x
    _ -> Nothing

-- | Parse an unsigned integer, yielding `Nothing` on any extranious characters.
parseInt :: Integral a => Text -> Maybe a 
parseInt t = case TR.decimal (T.strip t) of
    Right (x, "") -> Just x
    _ -> Nothing
