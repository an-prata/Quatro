{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Relude
import Obj
import Recognition
import Geometry

main :: IO ()
main = do
    [filePath] <- getArgs
    (obj :: [TriFace (Vertex Double)]) <- parseObj filePath
    putStrLn $ show (length obj) ++ " faces"
    let mesh = viaNonEmpty tileFaces obj
    print $ meshLevels <$> mesh
    print $ meshSize <$> mesh
    let mapped = mapMesh <$> mesh
    let edges = collectEdges <$> mapped
    print $ length <$> edges
    let ls = (`connectEdges` (45 / 180 * pi)) <$> edges
    print $ fmap isClosedLoop <$> ls
    let patches = mapMaybe patchFromLoop <$> ls
    let faces = facesFromPatches <$> patches
    print $ length <$> faces
