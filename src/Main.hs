{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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
    let mesh = fst <$> viaNonEmpty structureMesh obj
    putStrLn $ "Parsed mesh of size " ++ show (meshSize <$> mesh)

    let mapped = mesh >>= describeMesh
    let edges = collectEdges <$> mapped
    putStrLn $ "Found " ++ show (length <$> edges) ++ " edges"
    let ols = makeOutline (45 / 180 * pi) <$> mapped
    putStrLn $ "Created " ++ show (length <$> ols) ++ " lines"
    -- let patches = concatMap (mapMaybe patchFromLoop . outlineStrands) <$> ols
    -- putStrLn $ "Created " ++ show (length <$> patches) ++ " patches"
    -- let faces =  facesFromPatches <$> patches
    -- putStrLn $ "Created " ++ show (length <$> faces) ++ " faces"
    -- forM_ (writeQuads <$> faces) putText
