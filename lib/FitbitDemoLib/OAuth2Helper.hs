{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Helper
    ( UpdateTokenPair
    , oAuth2Get
    , withRefresh
    ) where

import           Control.Exception (catch, throwIO)
import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           FitbitDemoLib.HttpUtil
import           FitbitDemoLib.Types
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
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessToken
                    , App(..)
                    , ClientPair(..)
                    , RefreshTokenRequest(..)
                    , RefreshTokenResponse(..)
                    , TokenPair(..)
                    , fetchRefreshToken
                    , oAuth2BearerHeader
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

oAuth2Get :: Url 'Https -> OAuth2.AccessToken -> IO Value
oAuth2Get url accessToken =
    responseBody <$> (runReq def $ req GET url NoReqBody jsonResponse (OAuth2.oAuth2BearerHeader accessToken <> acceptLanguage))
