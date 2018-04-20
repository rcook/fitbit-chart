{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FitbitChart.App.Util.Util
    ( fitbitApiUrl
    , formatDouble
    , mkApp
    ) where

import           Network.HTTP.Req ((/:), Scheme(..), Url, https)
import           Network.HTTP.Req.OAuth2 (App(..), ClientPair, UpdateTokenPair)
import           Text.Printf (printf)
import           Text.URI.QQ (uri)

formatDouble :: Double -> String
formatDouble = printf "%.1f"

mkApp :: UpdateTokenPair -> ClientPair -> App
mkApp updateTokenPair clientPair =
    App
        [uri|https://www.fitbit.com/oauth2/authorize|]  -- appAuthUri
        [uri|https://api.fitbit.com/oauth2/token|]      -- appTokenUri
        updateTokenPair
        clientPair

fitbitApiUrl :: Url 'Https
fitbitApiUrl = https "api.fitbit.com" /: "1"
