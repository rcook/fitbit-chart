{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemo.Config
    ( AccessToken(..)
    , ClientId(..)
    , ClientSecret(..)
    , Config(..)
    , FitbitAPI(..)
    , RefreshToken(..)
    , TokenConfig(..)
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

data FitbitAPI = FitbitAPI ClientId ClientSecret deriving (Eq, Show)

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

data Config = Config FitbitAPI deriving (Eq, Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config <$> v .: "fitbit-api"

instance ToJSON Config where
    toJSON (Config fitbitAPI) =
        object
            [ "fitbit-api" .= fitbitAPI
            ]

newtype AccessToken = AccessToken Text deriving Show
newtype RefreshToken = RefreshToken Text deriving Show

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
