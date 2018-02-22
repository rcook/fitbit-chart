{-# LANGUAGE DataKinds #-}

module FitbitDemoApp.APIUtil
    ( fitbitApiGet
    , oAuth2Post
    ) where

import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( FormUrlEncodedParam
                    , GET(..)
                    , NoReqBody(..)
                    , Option
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , Scheme(..)
                    , Url
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )

fitbitApiGet :: Url 'Https -> TokenConfig -> IO Value
fitbitApiGet url tokenConfig =
    responseBody <$> (runReq def $ req GET url NoReqBody jsonResponse (bearerHeader tokenConfig <> acceptLanguage))

-- TODO: I think this is OAuth2-specific
oAuth2Post :: Url 'Https -> Option 'Https -> FormUrlEncodedParam -> IO Value
oAuth2Post url opts formBody = runReq def $ responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
