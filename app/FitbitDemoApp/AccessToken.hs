{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.AccessToken
    ( AccessTokenResponse(..)
    , doIt
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), Value, object, withObject)
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString as ByteString (append, concat)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           FitbitDemoApp.Util
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (/:)
                    , (=:)
                    , GET(..)
                    , HttpException(..)
                    , NoReqBody(..)
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , Scheme(..)
                    , Url
                    , header
                    , https
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )

data AccessTokenResponse = AccessTokenResponse AccessToken RefreshToken deriving Show

instance FromJSON AccessTokenResponse where
    parseJSON =
        withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
            <$> (AccessToken <$> v .: "access_token")
            <*> (RefreshToken <$> v .: "refresh_token")

doIt :: Url 'Https -> AuthCode -> ClientId -> ClientSecret -> IO (Either String AccessTokenResponse)
doIt url (AuthCode ac) clientId@(ClientId cid) clientSecret = runReq def $ do
    let opts = header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))
        formBody = "code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text)
    body <- responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    return $ parseEither parseJSON body
