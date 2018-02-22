{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Util
    ( bearerHeader
    ) where

import qualified Data.Text.Encoding as Text (encodeUtf8)
import           FitbitDemoLib.TokenConfig
import           Network.HTTP.Req (Option, Scheme(..), oAuth2Bearer)
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (AccessToken(..))

bearerHeader :: TokenConfig -> Option 'Https
bearerHeader (TokenConfig (OAuth2.AccessToken at) _) = oAuth2Bearer (Text.encodeUtf8 at)
