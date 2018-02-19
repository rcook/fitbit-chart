{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.FitbitAPI
    ( FitbitAPI(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           FitbitDemoLib.Types

-- | Fitbit API configuration
data FitbitAPI = FitbitAPI ClientId ClientSecret deriving Show

-- | 'FromJSON' instance for deserializing Fitbit API configuration from YAML
instance FromJSON FitbitAPI where
    parseJSON =
        withObject "FitbitAPI" $ \v -> FitbitAPI
            <$> (ClientId <$> v .: "client-id")
            <*> (ClientSecret <$> v .: "client-secret")

-- | 'ToJSON' instance for serializing Fitbit API configuration to YAML
instance ToJSON FitbitAPI where
    toJSON (FitbitAPI (ClientId cid) (ClientSecret cs)) =
        object
            [ "client-id" .= cid
            , "client-secret" .= cs
            ]
