{-# LANGUAGE DataKinds #-}

module FitbitDemoApp.APIUtil
    ( fitbitApiGet
    ) where

import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           FitbitDemoLib
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

fitbitApiGet :: Url 'Https -> TokenConfig -> IO Value
fitbitApiGet url tokenConfig =
    responseBody <$> (runReq def $ req GET url NoReqBody jsonResponse (bearerHeader tokenConfig <> acceptLanguage))
