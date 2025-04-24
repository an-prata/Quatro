{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Relude
import Obj
import Recognition
import Geometry
import Data.List (nub)
import qualified Data.Set as Set

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
    let ols = makeOutline (60 * pi / 180 ) <$> mapped
    putStrLn $ "Created " ++ show (length <$> ols) ++ " lines"
    let strands = concatMap strandsOf <$> ols
    putStrLn $ "Created " ++ show (length <$> strands) ++ " strands"
    -- let patches = fmap straightenStrand <$> strands
    -- putStrLn $ "Created " ++ show (length <$> patches) ++ " patches"
    let faces = nub . concatMap (strandQuads . toList) <$> strands
    putStrLn $ "Created " ++ show (length <$> faces) ++ " faces"

    -- print $ fmap length <$> patches
    forM_ (writeQuads <$> faces) putText

    case faces of
        Just fs -> do 
            putStrLn $ "Number of faces: " ++ show (length obj) ++ " -> " ++ show (length fs)
            putStrLn $ "Number of Vertices: " ++ show (Set.size $ uniquePoints obj) ++ " -> " ++ show (Set.size $ uniquePoints fs)
        Nothing -> putStrLn "Failed to make new mesh"
