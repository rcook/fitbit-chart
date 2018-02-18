{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           FitbitDemoApp

main :: IO ()
main = do
    Just config <- getConfig
    print config
