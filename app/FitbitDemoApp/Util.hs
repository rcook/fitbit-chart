{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.Util
    ( encodeClientAuth
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           FitbitDemoLib

encodeClientAuth :: ClientId -> ClientSecret -> ByteString
encodeClientAuth (ClientId cid) (ClientSecret cs) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cid, ":", Text.encodeUtf8 cs]
