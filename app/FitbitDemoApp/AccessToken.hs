{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.AccessToken
    ( AccessTokenRequest(..)
    , AccessTokenResponse(..)
    , sendAccessToken
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

data AccessTokenRequest = AccessTokenRequest FitbitAPI AuthCode

data AccessTokenResponse = AccessTokenResponse AccessToken RefreshToken

sendAccessToken :: App -> AccessTokenRequest -> IO (Either String AccessTokenResponse)
sendAccessToken app (AccessTokenRequest (FitbitAPI clientId@(ClientId cid) clientSecret) (AuthCode ac)) = do
    let Just (url, _) = toUrlHttps $ tokenUri app
    parseEither pResponse <$>
        oAuth2Post
            url
            (tokenAuthHeader clientId clientSecret)
            ("code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text))

pResponse :: Value -> Parser AccessTokenResponse
pResponse =
    withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")
