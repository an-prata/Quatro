{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Obj
    ( parseObj
    , parseTriFace
    ) where

import Relude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Geometry

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
