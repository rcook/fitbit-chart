{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.FitbitAPI
    ( FitbitAPI(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           FitbitDemoLib.Types

data FitbitAPI = FitbitAPI ClientId ClientSecret deriving Show

instance FromJSON FitbitAPI where
    parseJSON =
        withObject "FitbitAPI" $ \v -> FitbitAPI
            <$> (ClientId <$> v .: "client-id")
            <*> (ClientSecret <$> v .: "client-secret")

instance ToJSON FitbitAPI where
    toJSON (FitbitAPI (ClientId clientId) (ClientSecret clientSecret)) =
        object
            [ "client-id" .= clientId
            , "client-secret" .= clientSecret
            ]
