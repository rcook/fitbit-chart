{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.RefreshToken
    ( RefreshTokenResponse(..)
    , sendRefreshToken
    ) where

import           Data.Aeson ((.:), FromJSON(..), withObject)
import           Data.Aeson.Types (parseEither)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           FitbitDemoApp.Util
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (/:)
                    , (=:)
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , https
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )

data RefreshTokenResponse = RefreshTokenResponse AccessToken RefreshToken deriving Show

instance FromJSON RefreshTokenResponse where
    parseJSON =
        withObject "RefreshTokenResponse" $ \v -> RefreshTokenResponse
            <$> (AccessToken <$> v .: "access_token")
            <*> (RefreshToken <$> v .: "refresh_token")

sendRefreshToken :: ClientId -> ClientSecret -> RefreshToken -> IO (Either String RefreshTokenResponse)
sendRefreshToken clientId clientSecret (RefreshToken rt) = do
    let url = https "api.fitbit.com" /: "oauth2" /: "token"
        opts = tokenAuthHeader clientId clientSecret
        formBody = "grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: rt <> "expires_in" =: ("3600" :: Text)
    body <- runReq def $ responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    return $ parseEither parseJSON body
