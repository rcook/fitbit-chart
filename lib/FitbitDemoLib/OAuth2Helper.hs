{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Helper
    ( oAuth2Get
    , mkOAuth2Call
    ) where

import           Control.Exception (catch, throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (get, put)
import           Data.Aeson (Value)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           FitbitDemoLib.HttpUtil
import           FitbitDemoLib.OAuth2Types
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
import           Network.HTTP.Req.OAuth2 (OAuth2)
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessToken
                    , App(..)
                    , ClientPair(..)
                    , RefreshTokenRequest(..)
                    , RefreshTokenResponse(..)
                    , TokenPair(..)
                    , UpdateTokenPair
                    , fetchRefreshToken
                    , oAuth2BearerHeader
                    )
import           Network.HTTP.Types (unauthorized401)

mkOAuth2Call ::
    (OAuth2 (APIResult a) -> OAuth2 a)
    -> OAuth2.App
    -> Url 'Https
    -> APIAction a
    -> OAuth2 a
mkOAuth2Call f app apiUrl action = f $ wrap (action apiUrl app)
    where
        wrap a = do
            tp <- get
            (result, tp') <- liftIO $ a tp
            put tp'
            return result

oAuth2Get ::
    (Value -> Parser a)
    -> APIAction a
oAuth2Get p apiUrl app@(OAuth2.App _ _ u clientPair) tokenPair@(OAuth2.TokenPair accessToken _) = do
    (temp, tokenPair') <- catch (getHelper apiUrl accessToken >>= \value -> return (value, tokenPair)) $
                            \e -> if hasResponseStatus e unauthorized401
                                    then do
                                        newTokenPair@(OAuth2.TokenPair newAccessToken _) <- refreshHelper app tokenPair
                                        result <- getHelper apiUrl newAccessToken
                                        return (result, newTokenPair)
                                    else throwIO e
    return (parseEither p temp, tokenPair')

getHelper ::
    Url 'Https
    -> OAuth2.AccessToken
    -> IO Value
getHelper url accessToken =
    responseBody <$> (runReq def $ req GET url NoReqBody jsonResponse (OAuth2.oAuth2BearerHeader accessToken <> acceptLanguage))

refreshHelper ::
    OAuth2.App
    -> OAuth2.TokenPair
    -> IO OAuth2.TokenPair
refreshHelper app@(OAuth2.App _ _ u clientPair) (OAuth2.TokenPair _ refreshToken) = do
    result <- OAuth2.fetchRefreshToken app (OAuth2.RefreshTokenRequest refreshToken)
    let (OAuth2.RefreshTokenResponse newTokenPair) = case result of
                                                        Left e -> error e -- TODO: Error handling
                                                        Right x -> x
    u newTokenPair
    return newTokenPair
