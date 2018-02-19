{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.AppConfig
    ( AccessToken(..)
    , ClientId(..)
    , ClientSecret(..)
    , Config(..)
    , FitbitAPI(..)
    , RefreshToken(..)
    , TokenConfig(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           FitbitDemoLib.FitbitAPI
import           FitbitDemoLib.Types

data Config = Config FitbitAPI deriving Show

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config <$> v .: "fitbit-api"

instance ToJSON Config where
    toJSON (Config fitbitAPI) =
        object
            [ "fitbit-api" .= fitbitAPI
            ]

data TokenConfig = TokenConfig AccessToken RefreshToken deriving Show

instance FromJSON TokenConfig where
    parseJSON =
        withObject "TokenConfig" $ \v -> TokenConfig
            <$> (AccessToken <$> v .: "access-token")
            <*> (RefreshToken <$> v .: "refresh-token")

instance ToJSON TokenConfig where
    toJSON (TokenConfig (AccessToken at) (RefreshToken rt)) =
        object
            [ "access-token" .= at
            , "refresh-token" .= rt
            ]
