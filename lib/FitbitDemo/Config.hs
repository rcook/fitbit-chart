{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemo.Config
    ( ClientId(..)
    , Config(..)
    , FitbitAPI(..)
    , Secret(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           GHC.Generics (Generic)
import           Data.Text (Text)

newtype ClientId = ClientId Text deriving (Eq, Generic, Show)
instance FromJSON ClientId
instance ToJSON ClientId

newtype Secret = Secret Text deriving (Eq, Generic, Show)
instance FromJSON Secret
instance ToJSON Secret

data FitbitAPI = FitbitAPI ClientId Secret deriving (Eq, Generic, Show)
instance FromJSON FitbitAPI where
    parseJSON =
        withObject "FitbitAPI" $ \v -> FitbitAPI
            <$> v .: "client-id"
            <*> v .: "secret"

instance ToJSON FitbitAPI where
    toJSON (FitbitAPI (ClientId clientId) (Secret secret)) =
        object
            [ "client-id" .= clientId
            , "secret" .= secret
            ]

data Config = Config FitbitAPI deriving (Eq, Generic, Show)
instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config <$> v .: "fitbit-api"

instance ToJSON Config where
    toJSON (Config fitbitAPI) =
        object
            [ "fitbit-api" .= fitbitAPI
            ]
