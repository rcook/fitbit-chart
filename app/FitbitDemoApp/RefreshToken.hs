{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.RefreshToken
    ( RefreshTokenResponse(..)
    , sendRefreshToken
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
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.Url.Extra (toUrlHttps)

data RefreshTokenResponse = RefreshTokenResponse AccessToken RefreshToken

pResponse :: Value -> Parser RefreshTokenResponse
pResponse =
    withObject "RefreshTokenResponse" $ \v -> RefreshTokenResponse
        <$> (AccessToken <$> v .: "access_token")
        <*> (RefreshToken <$> v .: "refresh_token")

sendRefreshToken :: OAuth2App -> ClientId -> ClientSecret -> RefreshToken -> IO (Either String RefreshTokenResponse)
sendRefreshToken oauth2 clientId clientSecret (RefreshToken rt) = do
    let Just (url, _) = toUrlHttps $ tokenRequestURI oauth2
        opts = tokenAuthHeader clientId clientSecret
        formBody = "grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: rt <> "expires_in" =: ("3600" :: Text)
    body <- runReq def $ responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    return $ parseEither pResponse body
