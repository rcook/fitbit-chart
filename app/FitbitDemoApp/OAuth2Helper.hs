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
                    , TokenPair(..)
                    , fetchAccessToken
                    , fetchRefreshToken
                    )
import           Network.HTTP.Types (unauthorized401)

refresh :: OAuth2.App -> OAuth2.ClientId -> OAuth2.ClientSecret -> OAuth2.TokenPair -> IO OAuth2.TokenPair
refresh app clientId clientSecret (OAuth2.TokenPair _ refreshToken) = do
    result <- OAuth2.fetchRefreshToken app (OAuth2.RefreshTokenRequest clientId clientSecret refreshToken)
    let (OAuth2.RefreshTokenResponse at rt) = case result of
                                                Left e -> error e
                                                Right x -> x
    let newTokenPair = OAuth2.TokenPair at rt
    tokenPairPath <- getTokenPairPath
    encodeYAMLFile tokenPairPath (TokenPairWrapper newTokenPair)
    return newTokenPair

withRefresh :: OAuth2.App -> Url 'Https -> AppConfig -> OAuth2.TokenPair -> APIAction a -> IO (Either String a, OAuth2.TokenPair)
withRefresh app apiUrl (AppConfig (FitbitAPI clientId clientSecret)) tokenPair action =
    catch (action apiUrl tokenPair >>= \result -> return (result, tokenPair)) $
        \e -> if hasResponseStatus e unauthorized401
                then do
                    newTokenPair <- refresh app clientId clientSecret tokenPair
                    result <- action apiUrl newTokenPair
                    return (result, newTokenPair)
                else throwIO e
