{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.APIUtil
    ( fitbitApiGet
    ) where

import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           FitbitDemoLib.HttpUtil
import           Network.HTTP.Req
                    ( GET(..)
                    , NoReqBody(..)
                    , Scheme(..)
                    , Url
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (AccessToken, oAuth2BearerHeader)

fitbitApiGet :: Url 'Https -> OAuth2.AccessToken -> IO Value
fitbitApiGet url accessToken =
    responseBody <$> (runReq def $ req GET url NoReqBody jsonResponse (OAuth2.oAuth2BearerHeader accessToken <> acceptLanguage))
