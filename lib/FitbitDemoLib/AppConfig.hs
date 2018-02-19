{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.AppConfig
    ( AppConfig(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           FitbitDemoLib.FitbitAPI

-- | Application configuration
data AppConfig = AppConfig FitbitAPI deriving Show

-- | 'FromJSON' instance for deserializing application configuration from YAML
instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \v -> AppConfig <$> v .: "fitbit-api"

-- | 'ToJSON' instance for serializing application configuration to YAML
instance ToJSON AppConfig where
    toJSON (AppConfig fitbitAPI) =
        object
            [ "fitbit-api" .= fitbitAPI
            ]
