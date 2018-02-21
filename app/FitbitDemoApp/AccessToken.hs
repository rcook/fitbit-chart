{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.AccessToken
    ( AccessTokenRequest(..)
    , AccessTokenResponse(..)
    , sendAccessToken
    ) where

import           Data.Aeson ((.:), withObject)
import           Data.Aeson.Types (Parser, Value, parseEither)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (=:)
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.Url.Extra (toUrlHttps)
import           OAuth2

data AccessTokenRequest = AccessTokenRequest FitbitAPI AuthCode

data AccessTokenResponse = AccessTokenResponse AccessToken RefreshToken

pResponse :: Value -> Parser AccessTokenResponse
pResponse =
    withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")

sendAccessToken :: App -> AccessTokenRequest -> IO (Either String AccessTokenResponse)
sendAccessToken oauth2 (AccessTokenRequest (FitbitAPI clientId@(ClientId cid) clientSecret) (AuthCode ac)) = runReq def $ do
    let Just (url, _) = toUrlHttps $ tokenUri oauth2
        opts = tokenAuthHeader clientId clientSecret
        formBody = "code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text)
    body <- responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    return $ parseEither pResponse body
