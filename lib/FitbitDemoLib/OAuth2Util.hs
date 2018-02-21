{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.OAuth2Util
    ( tokenAuthHeader
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           FitbitDemoLib.Types
import           Network.HTTP.Req (Option, header)

encodeClientAuth :: ClientId -> ClientSecret -> ByteString
encodeClientAuth (ClientId cid) (ClientSecret cs) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cid, ":", Text.encodeUtf8 cs]

tokenAuthHeader :: ClientId -> ClientSecret -> Option scheme
tokenAuthHeader clientId clientSecret =
    header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))
