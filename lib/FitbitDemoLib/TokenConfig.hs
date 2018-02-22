{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.TokenConfig
    ( TokenConfig(..)
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (AccessToken(..), RefreshToken(..))

data TokenConfig = TokenConfig OAuth2.AccessToken OAuth2.RefreshToken deriving Show

instance FromJSON TokenConfig where
    parseJSON =
        withObject "TokenConfig" $ \v -> TokenConfig
            <$> (OAuth2.AccessToken <$> v .: "access-token")
            <*> (OAuth2.RefreshToken <$> v .: "refresh-token")

instance ToJSON TokenConfig where
    toJSON (TokenConfig (OAuth2.AccessToken at) (OAuth2.RefreshToken rt)) =
        object
            [ "access-token" .= at
            , "refresh-token" .= rt
            ]
