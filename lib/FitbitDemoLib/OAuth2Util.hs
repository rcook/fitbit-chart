{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.OAuth2Util
    ( bearerHeader
    , tokenAuthHeader
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           FitbitDemoLib.TokenConfig
import           Network.HTTP.Req (Option, Scheme(..), header, oAuth2Bearer)
import           OAuth2

encodeClientAuth :: ClientId -> ClientSecret -> ByteString
encodeClientAuth (ClientId cid) (ClientSecret cs) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cid, ":", Text.encodeUtf8 cs]

tokenAuthHeader :: ClientId -> ClientSecret -> Option scheme
tokenAuthHeader clientId clientSecret =
    header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))

bearerHeader :: TokenConfig -> Option 'Https
bearerHeader (TokenConfig (AccessToken at) _) = oAuth2Bearer (Text.encodeUtf8 at)
