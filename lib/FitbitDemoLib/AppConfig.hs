{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.AppConfig
    ( AppConfig(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           FitbitDemoLib.FitbitAPI

data AppConfig = AppConfig FitbitAPI deriving Show

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \v -> AppConfig <$> v .: "fitbit-api"

instance ToJSON AppConfig where
    toJSON (AppConfig fitbitAPI) =
        object
            [ "fitbit-api" .= fitbitAPI
            ]
