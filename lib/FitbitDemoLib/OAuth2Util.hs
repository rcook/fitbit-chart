{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Util
    ( bearerHeader
    , tokenAuthHeader
    ) where

import qualified Data.Text.Encoding as Text (encodeUtf8)
import           FitbitDemoLib.TokenConfig
import           Network.HTTP.Req (Option, Scheme(..), oAuth2Bearer)
import           OAuth2

bearerHeader :: TokenConfig -> Option 'Https
bearerHeader (TokenConfig (AccessToken at) _) = oAuth2Bearer (Text.encodeUtf8 at)
