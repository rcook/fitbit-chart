{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FitbitDemoApp.Util
    ( exitOnFailure
    , fitbitApiUrl
    , formatDouble
    , mkApp
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Network.HTTP.Req ((/:), Scheme(..), Url, https)
import           Network.HTTP.Req.OAuth2 (App(..), ClientPair, UpdateTokenPair)
import           System.Exit (exitFailure)
import           Text.Printf (printf)
import           Text.URI.QQ (uri)

exitOnFailure :: (MonadIO m) => m (Either String a) -> m a
exitOnFailure action = do
    result <- action
    case result of
        Left e -> liftIO (putStrLn e >> exitFailure)
        Right x -> return x

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
