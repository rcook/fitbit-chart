{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.TokenConfig
    ( TokenConfig(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           FitbitDemoLib.Types

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
