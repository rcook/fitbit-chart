{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.FitbitAPI
    ( FitbitAPI(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (ClientId(..), ClientPair(..), ClientSecret(..))

-- | Fitbit API configuration
data FitbitAPI = FitbitAPI OAuth2.ClientPair deriving (Eq, Show)

-- | 'FromJSON' instance for deserializing Fitbit API configuration from YAML
instance FromJSON FitbitAPI where
    parseJSON =
        withObject "FitbitAPI" $ \v -> (FitbitAPI .) . OAuth2.ClientPair
            <$> (OAuth2.ClientId <$> v .: "client-id")
            <*> (OAuth2.ClientSecret <$> v .: "client-secret")

-- | 'ToJSON' instance for serializing Fitbit API configuration to YAML
instance ToJSON FitbitAPI where
    toJSON (FitbitAPI (OAuth2.ClientPair (OAuth2.ClientId cid) (OAuth2.ClientSecret cs))) =
        object
            [ "client-id" .= cid
            , "client-secret" .= cs
            ]
