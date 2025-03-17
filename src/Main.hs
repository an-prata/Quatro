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
    putStrLn $ "Opened OBJ file with " ++ show (length obj) ++ " faces"
    let mesh = viaNonEmpty tileFaces obj
    putStrLn $ "Parsed mesh of size " ++ show (meshSize <$> mesh)
    let mapped = mapMesh <$> mesh
    let edges = collectEdges <$> mapped
    putStrLn $ "Found " ++ show (length <$> edges) ++ " edges"
    let unweighted = fmap (\(WeightedEdge _ e) -> e) <$> edges
    forM_ (writeEdges <$> unweighted) putText
    -- let ls = (`connectEdges` (45 / 180 * pi)) <$> edges
    -- putStrLn $ "Created " ++ show (length <$> ls) ++ " lines"
    -- let patches = minimizePatches . mapMaybe patchFromLoop <$> ls
    -- putStrLn $ "Created " ++ show (length <$> patches) ++ " patches"
    -- let faces =  facesFromPatches <$> patches
    -- putStrLn $ "Created " ++ show (length <$> faces) ++ " faces"
    -- forM_ (writeQuads <$> faces) putText
