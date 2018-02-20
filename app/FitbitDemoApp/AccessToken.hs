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
import           FitbitDemoApp.Util
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (=:)
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , Scheme(..)
                    , Url
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )

data AccessTokenRequest = AccessTokenRequest FitbitAPI AuthCode deriving Show

data AccessTokenResponse = AccessTokenResponse AccessToken RefreshToken deriving Show

pResponse :: Value -> Parser AccessTokenResponse
pResponse =
    withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")

sendAccessToken :: Url 'Https -> AccessTokenRequest -> IO (Either String AccessTokenResponse)
sendAccessToken url (AccessTokenRequest (FitbitAPI clientId@(ClientId cid) clientSecret) (AuthCode ac)) = runReq def $ do
    let opts = tokenAuthHeader clientId clientSecret
        formBody = "code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text)
    body <- responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    return $ parseEither pResponse body
