{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemo.Config
    ( ClientId(..)
    , Config(..)
    , FitbitConfig(..)
    , Secret(..)
    ) where

import           Data.Aeson ((.=), FromJSON(..), ToJSON(..), object)
import           GHC.Generics (Generic)
import           Data.Text (Text)

newtype ClientId = ClientId Text deriving (Generic, Show)
instance FromJSON ClientId
instance ToJSON ClientId
newtype Secret = Secret Text deriving (Generic, Show)
instance FromJSON Secret
instance ToJSON Secret

data FitbitConfig = FitbitConfig ClientId Secret deriving (Generic, Show)
instance FromJSON FitbitConfig
instance ToJSON FitbitConfig where
    toJSON (FitbitConfig (ClientId clientId) (Secret secret)) =
        object
            [ "client-id" .= clientId
            , "secret" .= secret
            ]

data Config = Config FitbitConfig deriving (Generic, Show)
instance ToJSON Config where
    toJSON (Config fitbitConfig) =
        object
            [ "fitbit-config" .= fitbitConfig
            ]