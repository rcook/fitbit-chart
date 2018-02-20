{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.Util
    ( hasResponseStatus
    , formatDay
    , mkDay
    , tokenAuthHeader
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           Data.Time.Calendar (Day, fromGregorian)
import qualified Network.HTTP.Client as HTTP (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Req (HttpException(..), Option, header)
import           Network.HTTP.Types (Status)
import           FitbitDemoLib

encodeClientAuth :: ClientId -> ClientSecret -> ByteString
encodeClientAuth (ClientId cid) (ClientSecret cs) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cid, ":", Text.encodeUtf8 cs]

tokenAuthHeader :: ClientId -> ClientSecret -> Option scheme
tokenAuthHeader clientId clientSecret =
    header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))

hasResponseStatus :: HttpException -> Status -> Bool
hasResponseStatus
    (VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _))) status =
    HTTP.responseStatus response == status
hasResponseStatus _ _ = False

mkDay :: Int -> Int -> Int -> Day
mkDay year month date = fromGregorian (fromIntegral year) month date

formatDay :: Day -> Text
formatDay = Text.pack . show
