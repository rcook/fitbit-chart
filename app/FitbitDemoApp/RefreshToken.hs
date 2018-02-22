{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.RefreshToken
    ( RefreshTokenResponse(..)
    , sendRefreshToken
    ) where

import           Data.Aeson ((.:), withObject)
import           Data.Aeson.Types (Parser, Value, parseEither)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           FitbitDemoApp.APIUtil
import           FitbitDemoLib
import           Network.HTTP.Req ((=:))
import           Network.HTTP.Req.Url.Extra (toUrlHttps)
import           OAuth2

data RefreshTokenResponse = RefreshTokenResponse AccessToken RefreshToken

sendRefreshToken :: App -> ClientId -> ClientSecret -> RefreshToken -> IO (Either String RefreshTokenResponse)
sendRefreshToken app clientId clientSecret (RefreshToken rt) = do
    let Just (url, _) = toUrlHttps $ tokenUri app
    parseEither pResponse <$>
        oAuth2Post
            url
            (tokenAuthHeader clientId clientSecret)
            ("grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: rt <> "expires_in" =: ("3600" :: Text))

pResponse :: Value -> Parser RefreshTokenResponse
pResponse =
    withObject "RefreshTokenResponse" $ \v -> RefreshTokenResponse
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")
