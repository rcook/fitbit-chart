{-# LANGUAGE DataKinds #-}

module FitbitDemoApp.OAuth2Helper
        ( withRefresh
        ) where

import           Control.Exception (catch, throwIO)
import           FitbitDemoApp.Config
import           FitbitDemoApp.Types
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (/:)
                    , Scheme(..)
                    , Url
                    , https
                    )
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessTokenRequest(..)
                    , AccessTokenResponse(..)
                    , App(..)
                    , ClientId(..)
                    , ClientSecret(..)
                    , PromptForCallbackURI
                    , RefreshTokenRequest(..)
                    , RefreshTokenResponse(..)
                    , fetchAccessToken
                    , fetchRefreshToken
                    )
import           Network.HTTP.Types (unauthorized401)

refresh :: OAuth2.App -> OAuth2.ClientId -> OAuth2.ClientSecret -> TokenConfig -> IO TokenConfig
refresh app clientId clientSecret (TokenConfig _ refreshToken) = do
    result <- OAuth2.fetchRefreshToken app (OAuth2.RefreshTokenRequest clientId clientSecret refreshToken)
    let (OAuth2.RefreshTokenResponse at rt) = case result of
                                                Left e -> error e
                                                Right x -> x
    let newTokenConfig = TokenConfig at rt
    tokenConfigPath <- getTokenConfigPath
    encodeYAMLFile tokenConfigPath newTokenConfig
    return newTokenConfig

withRefresh :: OAuth2.App -> Url 'Https -> AppConfig -> TokenConfig -> APIAction a -> IO (Either String a, TokenConfig)
withRefresh app apiUrl (AppConfig (FitbitAPI clientId clientSecret)) tokenConfig action =
    catch (action apiUrl tokenConfig >>= \result -> return (result, tokenConfig)) $
        \e -> if hasResponseStatus e unauthorized401
                then do
                    newTokenConfig <- refresh app clientId clientSecret tokenConfig
                    result <- action apiUrl newTokenConfig
                    return (result, newTokenConfig)
                else throwIO e
