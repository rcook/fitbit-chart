{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

--import System.Directory
--import System.FilePath
import FitbitDemo


foo :: FilePath -> IO ()
foo _ = do
    --homeDir <- getHomeDirectory
    --let configPath = homeDir </> d </> "config.yaml"
    {-
    result <- decodeFileEither configPath
    let config = case result of
                Left e -> error $ prettyPrintParseException e
                Right c -> c
    dumpConfig config
    -}
    let config = Config (FitbitConfig (ClientId "xyz") (Secret "abc"))
    print $ encodeYaml config
    print $ encodeJson config

main :: IO ()
main = do
    foo ".fitbit-demo"
