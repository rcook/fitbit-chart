{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Helper
    ( oAuth2Get
    , evalOAuth2App
    , mkOAuth2Call
    , runOAuth2App
    ) where

import           Control.Exception (catch, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put, runStateT)
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

mkOAuth2Call ::
    (StateT OAuth2.TokenPair IO (APIResult a) -> (StateT OAuth2.TokenPair IO a))
    -> App'
    -> Url 'Https
    -> (Url 'Https -> App' -> OAuth2.TokenPair -> IO (APIResult a, OAuth2.TokenPair))
    -> OAuth2App a
mkOAuth2Call f app apiUrl action = f $ wrap (action apiUrl app)
    where
        wrap a = do
            tp <- get
            (result, tp') <- liftIO $ a tp
            put tp'
            return result

evalOAuth2App :: OAuth2.TokenPair -> OAuth2App a -> IO a
evalOAuth2App = flip evalStateT

runOAuth2App :: OAuth2.TokenPair -> OAuth2App a -> IO (a, OAuth2.TokenPair)
runOAuth2App = flip runStateT

oAuth2Get ::
    (Value -> Parser a)
    -> Url 'Https
    -> App'
    -> OAuth2.TokenPair
    -> IO (APIResult a, OAuth2.TokenPair)
oAuth2Get p apiUrl (App' u app clientPair) tokenPair@(OAuth2.TokenPair accessToken _) = do
    (temp, tokenPair') <- catch (getHelper apiUrl accessToken >>= \value -> return (value, tokenPair)) $
                            \e -> if hasResponseStatus e unauthorized401
                                    then do
                                        newTokenPair@(OAuth2.TokenPair newAccessToken _) <- refreshHelper u app clientPair tokenPair
                                        result <- getHelper apiUrl newAccessToken
                                        return (result, newTokenPair)
                                    else throwIO e
    return (parseEither p temp, tokenPair')

getHelper :: Url 'Https -> OAuth2.AccessToken -> IO Value
getHelper url accessToken =
    responseBody <$> (runReq def $ req GET url NoReqBody jsonResponse (OAuth2.oAuth2BearerHeader accessToken <> acceptLanguage))

refreshHelper :: UpdateTokenPair -> OAuth2.App -> OAuth2.ClientPair -> OAuth2.TokenPair -> IO OAuth2.TokenPair
refreshHelper u app clientPair (OAuth2.TokenPair _ refreshToken) = do
    result <- OAuth2.fetchRefreshToken app (OAuth2.RefreshTokenRequest clientPair refreshToken)
    let (OAuth2.RefreshTokenResponse newTokenPair) = case result of
                                                        Left e -> error e -- TODO: Error handling
                                                        Right x -> x
    u newTokenPair
    return newTokenPair
