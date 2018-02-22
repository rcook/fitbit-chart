{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Helper
    ( UpdateTokenPair
    , withRefresh
    ) where

import           Control.Exception (catch, throwIO)
import           FitbitDemoLib.HttpUtil
import           FitbitDemoLib.Types
import           Network.HTTP.Req
                    ( Scheme(..)
                    , Url
                    )
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( App(..)
                    , ClientPair(..)
                    , RefreshTokenRequest(..)
                    , RefreshTokenResponse(..)
                    , TokenPair(..)
                    , fetchRefreshToken
                    )
import           Network.HTTP.Types (unauthorized401)

type UpdateTokenPair = OAuth2.TokenPair -> IO ()

refresh :: UpdateTokenPair -> OAuth2.App -> OAuth2.ClientPair -> OAuth2.TokenPair -> IO OAuth2.TokenPair
refresh u app clientPair (OAuth2.TokenPair _ refreshToken) = do
    result <- OAuth2.fetchRefreshToken app (OAuth2.RefreshTokenRequest clientPair refreshToken)
    let (OAuth2.RefreshTokenResponse newTokenPair) = case result of
                                                        Left e -> error e -- TODO: Error handling
                                                        Right x -> x
    u newTokenPair
    return newTokenPair

withRefresh :: UpdateTokenPair -> OAuth2.App -> Url 'Https -> OAuth2.ClientPair -> OAuth2.TokenPair -> APIAction a -> IO (Either String a, OAuth2.TokenPair)
withRefresh u app apiUrl clientPair tokenPair action =
    catch (action apiUrl tokenPair >>= \result -> return (result, tokenPair)) $
        \e -> if hasResponseStatus e unauthorized401
                then do
                    newTokenPair <- refresh u app clientPair tokenPair
                    result <- action apiUrl newTokenPair
                    return (result, newTokenPair)
                else throwIO e
