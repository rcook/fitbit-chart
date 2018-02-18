{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemo.Config
    ( ClientId(..)
    , ClientSecret(..)
    , Config(..)
    , FitbitAPI(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           GHC.Generics (Generic)
import           Data.Text (Text)

newtype ClientId = ClientId Text deriving (Eq, Generic, Show)
instance FromJSON ClientId
instance ToJSON ClientId

newtype ClientSecret = ClientSecret Text deriving (Eq, Generic, Show)
instance FromJSON ClientSecret
instance ToJSON ClientSecret

data FitbitAPI = FitbitAPI ClientId ClientSecret deriving (Eq, Generic, Show)
instance FromJSON FitbitAPI where
    parseJSON =
        withObject "FitbitAPI" $ \v -> FitbitAPI
            <$> v .: "client-id"
            <*> v .: "client-secret"

instance ToJSON FitbitAPI where
    toJSON (FitbitAPI (ClientId clientId) (ClientSecret clientSecret)) =
        object
            [ "client-id" .= clientId
            , "client-secret" .= clientSecret
            ]

data Config = Config FitbitAPI deriving (Eq, Generic, Show)
instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config <$> v .: "fitbit-api"

instance ToJSON Config where
    toJSON (Config fitbitAPI) =
        object
            [ "fitbit-api" .= fitbitAPI
            ]
